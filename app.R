library(rfm)
library(fable)
library(purrr)
library(furrr)
library(shiny)
library(feasts)
library(scales)
library(tsibble)
library(stringr)
library(lubridate)
library(tidyverse)
library(data.table)
library(strucchange)
library(future.apply)
library(shinydashboard)
library(dashboardthemes)

source("functions.R")

p_value_threshold <- 0.05

plot_font_size <- 20

# Data reading, cleaning and modeling -------------------------------------

# Get paths of all input files and load them
files <- paste0("Data/", list.files("Data/", pattern = "*.csv"))

data_by_invoice <- map(files,
                       ~fread(.x) %>% 
                         as_tibble()) %>% 
  reduce(bind_rows)

# RFM analysis to segment customers
rfm_result <- data_by_invoice %>% 
  mutate(date = as.Date(InvoiceDate),
         revenue = Quantity * Price) %>% 
  rfm_table_order(StockCode,
                  date,
                  revenue,
                  max(as.Date(.$InvoiceDate)))

# Extract RFM table
rfm_table <- rfm_result$rfm

# Make RFM heat map
rfm_plot <- rfm_heatmap(rfm_result, print_plot = FALSE) +
  ggtitle("Product level RFM") +
  theme(text = element_text(colour = "#DAD4D4"),
        panel.grid = element_line(colour = "#2D3741"),
        panel.background = element_rect(fill = "#2D3741"),
        axis.text = element_text(colour = "#BCB1B1", size = plot_font_size),
        plot.background = element_rect(fill = "#2D3741", color = "transparent"),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.box.margin = margin(t = 13),
        legend.background = element_rect(fill = "#2D3741"),
        legend.text = element_text(size = plot_font_size),
        legend.title = element_text(size = plot_font_size),
        plot.title = element_text(size = plot_font_size),
        axis.title = element_text(size = plot_font_size))

data_unified_names <- data_by_invoice %>% 
  mutate(yearmonth = yearmonth(as.Date(InvoiceDate)),
         # Combine same products with different colors
         product = str_sub(StockCode, 1, 5))

# Harmonizing the product names
description_names <- data_unified_names %>% 
  # Remove those without numbers in product ID
  filter(!str_detect(product, "^[[:alpha:]]")) %>% 
  select(Description, product, yearmonth) %>% 
  arrange(product, yearmonth) %>% 
  group_by(product) %>% 
  distinct(Description, product) %>% 
  # Remove different writing conventions, entry and migration mistakes
  filter(!str_detect(Description, "[:lower:]") & 
           str_detect(Description, "[:upper:]") &
           !str_detect(Description, "mazon") &
           !str_detect(Description, "djustment")) %>% 
  mutate(product = as.integer(product))

# Unique name combination
unique_names <- description_names %>% 
  distinct(product, .keep_all = TRUE)

# Product descriptions that appear more than once for one unique product ID
diff_names <- anti_join(description_names, unique_names)

# Apply cleaning by string intersection between multiple product names
clean_names <- map(unique_names$product,
                   ~intersect(
                     strsplit(unique_names %>%
                                as_tibble() %>% 
                                filter(product == .x) %>%
                                pull(Description),
                              split = " ")[[1]],
                     str_replace_all(strsplit(diff_names %>% 
                                                as_tibble() %>% 
                                                filter(product == .x) %>% 
                                                select(Description) %>% 
                                                as.character(),
                                              split = " ")[[1]],
                                     "[^[A-Z]]", "")) %>% 
                     paste(collapse = " ") %>% 
                     as_tibble()) %>% 
  reduce(rbind) %>% 
  cbind(unique_names) %>% 
  mutate(product_name = ifelse(value != "", value, Description)) %>% 
  select(product, product_name) %>% 
  as_tibble()

# Aggregate, calculate needed variables and keep products with enough data
data_monthly <- data_unified_names %>% 
  filter(Quantity > 0,
         Price > 0,
         # Remove those without numbers in product ID
         !str_detect(product, "^[[:alpha:]]")) %>% 
  group_by(yearmonth, product) %>% 
  # Calculate amount of sales, average prices, revenue and receipt amount
  summarise(quantity_sum = sum(Quantity, na.rm = TRUE),
            price_mean = weighted.mean(Price, Quantity, na.rm = TRUE),
            revenue = sum(Price * Quantity),
            n_receipts = n()) %>% 
  group_by(product) %>% 
  # Keep only products with two years of observations or more
  mutate(n_months = n()) %>% 
  ungroup() %>% 
  filter(n_months >= 24) %>% 
  arrange(product, yearmonth)

# Remove products with significant breaks in the data
data_to_arima <- data_monthly %>% 
  group_by(product) %>% 
  do(efp = efp(.$price_mean ~ 1,
               type = "Rec-CUSUM")) %>% 
  mutate(p_value = sctest(efp)$p) %>% 
  filter(p_value > p_value_threshold) %>%
  select(product) %>% 
  inner_join(data_monthly) %>%
  as_tsibble(key = "product", index = "yearmonth") %>% 
  fill_gaps()

# Train ARIMA models
models <- data_to_arima %>% 
  model(ARIMA(quantity_sum ~ price_mean))

# Shiny components --------------------------------------------------------

ui <- dashboardPage(
  title = "Sales dashboard",
  dashboardHeader(),
  dashboardSidebar(
    selectizeInput("products", "Select products",
                   choices = sort(unique(data_to_arima$product)),
                   multiple = TRUE),
    actionButton("run_optimization", "Run price optimization"),
    hr(),
    sidebarMenu(id = "menu",
                sidebarMenuOutput("sidebar")
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    
    tabItems(
      tabItem(tabName = "rfm",
              fluidRow(
                column(12,
                       plotOutput("rfm_plot"),
                       align = "center"),
                tags$head(tags$style(HTML('.row {width: 90%;}'))))
      ),
      tabItem(tabName = "results",
              fluidRow(
                column(12,
                       plotOutput("test_plot"),
                       align = "center"),
                tags$head(tags$style(HTML('.row {width: 90%;}'))))
      )
    )
  )
)

server <- function(input, output, session){
  # Menu before optimizing
  output$sidebar <- renderMenu({
    sidebarMenu(id = "menu",
                menuItem("RFM", tabName = "rfm")
    )
  })
  
  update_data <- eventReactive(input$run_optimization, {
    suppressMessages(
      # Print to suppress message about groups from ggplot
      print(
        plot_quantity_forecasts(
          get_optimal_prices(input$products),
          input$products)
      )
    )
    })
  
  observeEvent(input$run_optimization, {
    output$info_boxes <- renderUI({
    fluidRow(
      infoBox("Optimized revenue", "$1000", "Description", icon("dollar-sign")),
      infoBox("Revenue increasement", "10%", "Description", icon("percent")),
      infoBox("Something", 200, "Description", icon("chart-line"))
    )
    })
    
    output$sidebar <- renderMenu({
      sidebarMenu(id = "menu",
                  menuItem("RFM", tabName = "rfm"),
                  menuItem("Results", tabName = "results")
                  )
    })
    
    updateTabItems(session, "menu", "results")
  })
  
  output$rfm_plot <- renderPlot({
    rfm_plot
  }, height = 600, width = 750)
  output$test_plot <- renderPlot({
    update_data()
  })
}

shinyApp(ui, server)
