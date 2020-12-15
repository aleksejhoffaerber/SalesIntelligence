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
library(patchwork)
library(lubridate)
library(tidyverse)
library(shinyalert)
library(data.table)
library(strucchange)
library(future.apply)
library(shinydashboard)
library(shinycssloaders)
library(dashboardthemes)

# English month names in deployment
Sys.setlocale("LC_ALL","C")

plot_font_size <- 20

# For structural breaks
p_value_threshold <- 0.05

source("functions.R")
# source("modeling.R")

# Read RDS instead of running models in Shiny server
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
rfm_monetary_segments <- segments %>%
  group_by(segment) %>%
  summarise(median = median(amount)) %>%
  arrange(desc(median)) %>%
  # Reorder to show product segment hierarchy
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
      # Css to adjust sidebar spacing and coloring
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
    
    useShinyalert(),
    
    # Main content
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
                       plotOutput(outputId = "combination_plot",
                                  width = "1200px",
                                  height = "700px") %>% 
                         withSpinner(type = 7),
                       align = "center"),
                tags$head(tags$style(HTML(
                  paste0('.row {width: 90%;}',
                         '.info-box-content {text-align: left;}')))))
      )
    )
  )
)

server <- function(input, output, session){
  # Menu before optimizing without results tab
  output$sidebar <- renderMenu({
    sidebarMenu(id = "menu",
                menuItem("RFM", tabName = "rfm"),
                menuItem("Segments", tabName = "segments")
    )
  })
  
  # Static RFM and RFM segment plots
  output$rfm_plot <- renderPlot({
    rfm_plot
  }, height = 600, width = 750)
  
  output$rfm_monetary_segments <- renderPlot({
    rfm_monetary_segments
  }, height = 600, width = 750)
  
  # Filter functionality according to RFM segment
  observeEvent(input$segments, 
               updateSelectizeInput(session,
                                    "product_name",
                                    choices = data_to_arima %>% 
                                      filter(segment %in% input$segments) %>% 
                                      pull(product_name) %>% 
                                      unique())
  )
  
  # Optimization on button click
  observeEvent(input$run_optimization, {
    
    # Optimizing popup
    shinyalert("Optimizing...",
               type = "info",
               closeOnEsc = FALSE,
               showConfirmButton = FALSE)
    
    # Update sidebar to include results tab
    output$sidebar <- renderMenu({
      sidebarMenu(id = "menu",
                  menuItem("RFM", tabName = "rfm"),
                  menuItem("Segments", tabName = "segments"),
                  menuItem("Results", tabName = "results")
      )
    })
    
    # Make forecasts and get the optimal one
    forecasts <- get_forecasts(input$product_name, data_to_arima, models)
    optimal_forecast <- get_optimal_forecast(forecasts)
    
    # Boxes with information about the results
    output$info_boxes <- renderUI({
      fluidRow(
        infoBox("Optimized price",
                paste0(intToUtf8(163),
                       number_format(0.01)(
                         optimal_forecast$new_price)),
                "For the next month",
                icon("balance-scale")),
        infoBox("Optimized revenue",
                paste0(intToUtf8(163),
                       number_format(0.01)(
                         optimal_forecast$pred_revenue)),
                paste0("Up from ",
                       paste0(intToUtf8(163),
                              number_format(0.01)(
                                optimal_forecast$pred_revenue_normal))),
                icon("pound-sign")),
        infoBox("Revenue increasement", 
                percent_format(0.1)(optimal_forecast$pred_revenue / 
                                      optimal_forecast$pred_revenue_normal - 1)
                , "Compared to no optimization",
                icon("percent"))
      )
    })
    
    # Three plots together
    output$combination_plot <- renderPlot({
      p1 <- isolate(plot_revenue_forecasts(optimal_forecast,
                                           input$product_name,
                                           data_to_arima,
                                           plot_font_size))
      
      p2 <- isolate(plot_quantity_forecasts(optimal_forecast,
                                            input$product_name,
                                            data_to_arima,
                                            plot_font_size))
      
      p3 <- isolate(plot_revenue_price(forecasts,
                                       input$product_name,
                                       data_to_arima,
                                       plot_font_size))
      
      # Plot without warnings
      suppressMessages(
        print(
          ((p1 / p2) | p3) +
            plot_annotation(
              "Effect of price optimization",
              theme = theme(
                text = element_text(colour = "#DAD4D4"),
                plot.background = element_rect(fill = "#2D3741",
                                               color = "transparent"),
                plot.title = element_text(size = plot_font_size)))))
    })
    
    # Switch tab to results after optimizing
    updateTabItems(session, "menu", "results")
    
    # Close optimizing popup
    closeAlert()
    
    # Alert if forecasts unavailable
    if(length(optimal_forecast$pred_revenue) == 0){
      shinyalert(
        "Forecasts or optimization could not be done for this product",
        type = "error")
    }
    
  })
}

shinyApp(ui, server)
