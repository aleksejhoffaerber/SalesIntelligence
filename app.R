library(rfm)
library(fable)
library(purrr)
library(furrr)
library(shiny)
library(feasts)
library(scales)
library(future)
library(tsibble)
library(stringr)
library(lubridate)
library(tidyverse)
library(data.table)
library(strucchange)
library(future.apply)
library(shinydashboard)
library(shinycssloaders)
library(dashboardthemes)

p_value_threshold <- 0.05

plot_font_size <- 20

source("functions.R")
# source("modeling.R")

models <- readRDS("Data/models.RDS")
segments <- readRDS("Data/segments.RDS")
rfm_result <- readRDS("Data/rfm_results.RDS")
data_to_arima <- readRDS("Data/data_to_arima.RDS")

# Plotting ----------------------------------------------------------------
# Make RFM heat map
rfm_plot <- rfm_heatmap(rfm_result, print_plot = FALSE) +
  # Fix missing tiles
  geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0.5, ymax = 5.5),
            fill = "#F1EEF6") +
  geom_tile(aes(frequency_score, recency_score, fill = monetary)) +
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

# Segment plot and monetary contribution
# Reorder to show product segment hierarchy
rfm_monetary_segments <- segments %>%
  group_by(segment) %>%
  summarise(median = median(amount)) %>%
  arrange(desc(median)) %>%
  mutate(segment = fct_reorder(segment, median)) %>%
  ggplot(aes(x = median, y = segment, fill = segment)) +
  geom_col() +
  scale_fill_brewer(palette = "Blues") +
  ggtitle("Median monetary value by product segment") +
  xlab("Median monetary value") +
  ylab(NULL) +
  theme(legend.position = "none",
        text = element_text(colour = "#DAD4D4"),
        panel.grid = element_line(colour = "#2D3741"),
        panel.background = element_blank(),
        axis.text = element_text(colour = "#BCB1B1", size = plot_font_size),
        plot.background = element_rect(fill = "#2D3741", color = "transparent"),
        plot.title = element_text(size = plot_font_size),
        axis.title = element_text(size = plot_font_size))

# Shiny components --------------------------------------------------------

ui <- dashboardPage(
  title = "Sales dashboard",
  dashboardHeader(title = "Price optimization"),
  dashboardSidebar(
    selectizeInput("segments", "Filter products by segment",
                   choices = unique(data_to_arima$segment),
                   multiple = TRUE),
    selectizeInput("product_name", "Select product to optimize",
                   choices = sort(unique(data_to_arima$product_name)),
                   multiple = FALSE) %>% 
      tagAppendAttributes(class = "larger"),
    actionButton("run_optimization", "Run price optimization"),
    hr(),
    sidebarMenu(id = "menu",
                sidebarMenuOutput("sidebar")
    ),
    tags$head(tags$style(HTML(
      paste('.row {width: 90%;}',
            '#product_name+ div>.selectize-input {height: 60px !important;',
            'padding-top: 0px !important}',
            '#segments+ div>.selectize-input',
            '{margin-bottom: 0px; padding-top: 2px !important}',
            '.larger {padding-top: 0px !important}',
            '#run_optimization {background-color: #00C0EF; color: #FFFFFF}',
            '.shiny-bound-input.action-button {margin: auto !important}'
      ))))
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
      tabItem(tabName = "segments",
              fluidRow(
                column(12,
                       plotOutput("rfm_monetary_segments"),
                       align = "center"),
                tags$head(tags$style(HTML('.row {width: 90%;}'))))
      ),
      tabItem(tabName = "results",
              fluidRow(
                column(12,
                       uiOutput("info_boxes"),
                       plotOutput(outputId = "revenue_plot",
                                  width = "750px",
                                  height = "300px") %>% 
                         withSpinner(type = 7),
                       align = "center"),
                tags$head(tags$style(HTML(
                  paste0('.row {width: 90%;}',
                         '.info-box-content {text-align: left;}'))))),
              
              fluidRow(
                column(12,
                       plotOutput(outputId = "demand_plot",
                                  width = "750px",
                                  height = "300px") %>% 
                         withSpinner(type = 7),
                       align = "center"),
                tags$head(tags$style(HTML('.row {width: 90%;}')))),
              
              fluidRow(
                column(12,
                       plotOutput(outputId = "price_rev_plot",
                                  width = "750px",
                                  height = "300px") %>% 
                         withSpinner(type = 7),
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
                menuItem("RFM", tabName = "rfm"),
                menuItem("Segments", tabName = "segments")
    )
  })
  
  update_data <- eventReactive(input$run_optimization, {
    suppressMessages(
      # Print to suppress message about groups from ggplot
      print(
        get_forecasts(input$product_name, data_to_arima, models) %>% 
          get_optimal_forecast() %>% 
          plot_revenue_forecasts(input$product_name,
                                 data_to_arima,
                                 plot_font_size)
      )
    )
    })
  
  demand_forecast <- eventReactive(input$run_optimization, {
    suppressMessages(
      # Print to suppress message about groups from ggplot
      print(
        get_forecasts(input$product_name, data_to_arima, models) %>% 
          get_optimal_forecast() %>% 
          plot_quantity_forecasts(input$product_name,
                                  data_to_arima,
                                  plot_font_size)
      )
    )
  })
  
  price_revenue <- eventReactive(input$run_optimization, {
    suppressMessages(
      # Print to suppress message about groups from ggplot
      print(
        get_forecasts(input$product_name, data_to_arima, models) %>% 
          plot_revenue_price(input$product_name, data_to_arima, plot_font_size)
      )
    )
  })
  
  observeEvent(input$segments, 
               updateSelectizeInput(session,
                                    "product_name",
                                    choices = data_to_arima %>% 
                                      filter(segment %in% input$segments) %>% 
                                      pull(product_name) %>% 
                                      unique())
  )
  
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
                  menuItem("Segments", tabName = "segments"),
                  menuItem("Results", tabName = "results")
                  )
    })

    # Switch tab to results after optimizing
    updateTabItems(session, "menu", "results")
  })
  
  output$rfm_plot <- renderPlot({
    rfm_plot
  }, height = 600, width = 750)
  
  output$rfm_monetary_segments <- renderPlot({
    rfm_monetary_segments
  }, height = 600, width = 750)
  
  output$revenue_plot <- renderPlot({
    update_data()
  })
  
  output$demand_plot <- renderPlot({
    demand_forecast()
  })
  output$price_rev_plot <- renderPlot({
    price_revenue()
  })
}

shinyApp(ui, server)
c("BIG DOUGHNUT FRIDGE MAGNETS")
