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
library(shinydashboard)

source("functions.R")

p_value_threshold <- 0.05

# Data reading and cleaning -----------------------------------------------

# Get paths of all input files and load them
files <- paste0("Data/", list.files("Data/", pattern = "*.csv"))

data_by_invoice <- map(files,
                       ~fread(.x) %>% 
                         as_tibble()) %>% 
  reduce(bind_rows)

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
    fluidRow(
      plotOutput("test_plot")
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
