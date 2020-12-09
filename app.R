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

# Data reading and cleaning -----------------------------------------------

# Get paths of all input files and load them
files <- paste0("Data/", list.files("Data/", pattern = "*.csv"))

data_by_invoice <- map(files,
                       ~fread(.x) %>% 
                         as_tibble()) %>% 
  reduce(bind_rows)

# RFM analysis to segment customers 
# we also need to add a view for the products
rfm_result <- data_by_invoice %>% 
  mutate(date = as.Date(InvoiceDate),
         revenue = Quantity * Price) %>% 
  rfm_table_order(StockCode,
                  date,
                  revenue,
                  max(as.Date(.$InvoiceDate)))

# Extract RFM table
rfm_table <- rfm_result$rfm

# harmonizing the product names
description_names <- data_by_invoice %>% 
  mutate(yearmonth = yearmonth(as.Date(InvoiceDate)),
         # Combine same products with different colors
         product = str_sub(StockCode, 1, 5)) %>% 
  # Remove those without numbers in product ID
  filter(!str_detect(product, "^[[:alpha:]]")) %>% 
  select(Description, product, yearmonth) %>% 
  arrange(product, yearmonth) %>% 
  group_by(product) %>% 
  distinct(Description, product) %>% 
  # filter out different writing conventions, entry and migration mistakes (e.g. from amazon)
  filter(!grepl("[a-z]", Description) & 
           grepl("[[:upper:]]", Description) &
           !grepl("+mazon", Description) & 
           !grepl("+djustment", Description))

# unique name combination
unique_names <- description_names %>% 
  distinct(product, .keep_all = T)

# product descriptions that appear more than once for one unique product ID
diff_names <- anti_join(description_names, unique_names)

# apply cleaning by string intersection between multiple product names
clean_names <- map(as.integer(unique_names$product), ~intersect(
  strsplit(
    unique_names %>%
      as.data.frame() %>% 
      filter(product == .x) %>%
      select(Description) %>% 
      as.character(), split = " ")[[1]],
  as.character(str_replace_all(strsplit(
    diff_names %>% 
      as.data.frame() %>% 
      filter(product == .x) %>% 
      select(Description) %>% 
      as.character(), split = " ")[[1]], "[^[A-Z]]", ""))))

# change empty lists to keep interpretability for rbind
clean_names[lengths(clean_names) == 0] <- ""

# flatten list and their entries  
clean_names <- map(1:length(clean_names), 
                   ~do.call(paste, c(as.list(unlist(clean_names[[.x]], recursive = F)), sep = " ")) %>%  
                     as.data.frame)

# combine clean names with previous unique product names
clean_names <- do.call(rbind, lapply(clean_names, function(x) if (length(x) == 0) "" else x)) %>% 
  as.data.frame %>% 
  cbind(unique_names)

# conditional variable to prefer new, clean product name
clean_names <- clean_names %>% 
  cbind(sapply(1:nrow(clean_names), 
               function(x) if 
               (clean_names[x,1] == "") 
                 clean_names[x,2] else 
                   clean_names[x,1])) %>% 
  select(-., -Description) %>% 
  as.tibble()

colnames(clean_names) <- c("product", "product_name")


# Country Wise
data_monthly <- data_by_invoice %>%
  mutate(yearmonth = yearmonth(as.Date(InvoiceDate)),
         # Combine same products with different colors
         product = str_sub(StockCode, 1, 5)) %>% 
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

# Remove products with significant breaks
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
  # Force intercept
  model(ARIMA(quantity_sum ~ 1 + price_mean))

ui <- dashboardPage(
  title = "Sales dashboard",
  dashboardHeader(),
  dashboardSidebar(
    selectizeInput("products", "Select products",
                   choices = sort(unique(data_to_arima$product)),
                   multiple = TRUE),
    actionButton("run_optimization", "Run price optimization")
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    fluidRow(
      infoBox("Optimized revenue", "$1000", "Description", icon("dollar-sign")),
      infoBox("Revenue increasement", "10%", "Description", icon("percent")),
      infoBox("Something", 200, "Description", icon("chart-line")),
      
    ),
    fluidRow(
      plotOutput("test_plot"),
      tags$head(tags$style(HTML('.row {width: 90%;}')))
    )
  )
)

server <- function(input, output){
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
  
  output$test_plot <- renderPlot({
    update_data()
  })
}

shinyApp(ui, server)
