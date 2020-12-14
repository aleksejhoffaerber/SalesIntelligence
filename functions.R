# Translate product name to product id
translate_input <- function(input, data_to_arima) {
  data_to_arima %>% 
    filter(product_name %in% input) %>% 
    pull(product) %>% 
    head(1)
}

# Make forecasts for different prices
get_forecasts <- function(chosen_product, data_to_arima, models){
  # Translate product name to product id
  chosen_product <- translate_input(chosen_product, data_to_arima)
  
  # Make price data for forecasts
  new_data <- data_to_arima %>% 
    filter(product == chosen_product) %>% 
    # Get the last observation
    slice(n()) %>%
    # One month ahead
    mutate(yearmonth = yearmonth + 1,
           original_price = price_mean,
           # Price can vary +- 30 percent
           price_mean = list(c(seq(price_mean,
                                   price_mean * 1.3,
                                   0.01),
                               seq(price_mean - 0.01,
                                   price_mean * 0.7,
                                   -0.01)))) %>% 
    unnest(price_mean) %>% 
    # Filter out negative prices just in case
    filter(price_mean > 0) %>% 
    ungroup()
  
  # Get the corresponding model
  model_to_use <- models %>% 
    filter(product == new_data %>% 
             slice(1) %>% 
             pull(product))
  
  # Make forecasts for different prices
  forecasts <- future_map(1:nrow(new_data),
             ~model_to_use %>%  
               forecast(new_data %>% 
                          slice(.x) %>% 
                          as_tsibble(key = product, 
                                     index = yearmonth)) %>% 
               as_tibble()) %>% 
    reduce(bind_rows) %>% 
    group_by(product) %>% 
    select(product, 
           yearmonth, 
           quantity_sum,
           new_price = price_mean, 
           pred_quantity = .mean, 
           original_price) %>% 
    # Join original sales quantities
    inner_join(new_data %>% 
                 group_by(product) %>% 
                 slice(n()) %>% 
                 select(product,
                        yearmonth,
                        original_quantity = quantity_sum)) %>% 
    # Calculate revenues
    mutate(original_revenue = original_quantity * original_price,
           pred_revenue = pred_quantity * new_price) %>% 
    # Filter out negative quantities just in case
    filter(pred_quantity >= 0) %>%
    # Select those with highest predicted quantities
    arrange(-pred_revenue)
  
  # Join normal forecast without optimized price
  models %>% 
    filter(product == new_data %>% 
             slice(1) %>% 
             pull(product)) %>%
    forecast(data_to_arima %>% 
               filter(product == chosen_product) %>% 
               # Get the last observation
               slice(n()) %>%
               # One month ahead
               mutate(yearmonth = yearmonth + 1)) %>% 
    # Non-negative
    mutate(pred_quantity_normal = ifelse(.mean < 0, 0, .mean)) %>% 
    as_tsibble() %>% 
    select(product, yearmonth, pred_quantity_normal) %>% 
    as_tibble() %>% 
    inner_join(forecasts) %>% 
    # Predicted normal revenue without price optimization
    mutate(pred_revenue_normal = pred_quantity_normal * original_price)
}

# Obtain optimal prices to maximize sales
get_optimal_forecast <- function(forecasts){
  # Get optimal prices by maximizing revenue
  forecasts %>% 
    slice(1) %>% 
    as_tsibble(key = "product")
}

# Plots revenue vs time
plot_revenue_forecasts <- function(optimal_forecast,
                                   chosen_product,
                                   data_to_arima,
                                   plot_font_size){
  
  # Keep current product_name for later naming
  product_name <- chosen_product
  
  # Translate product_name to product_id
  chosen_product <- translate_input(chosen_product, data_to_arima)
  prod
  optimal_forecast %>% 
    autoplot(pred_revenue) +
    geom_point(color = "#DAD4D4") +
    autolayer(optimal_forecast, pred_revenue_normal) +
    geom_point(aes(y = pred_revenue_normal), color = "#369093") +
    geom_segment(aes(xend = yearmonth,
                     y = pred_revenue_normal,
                     yend = pred_revenue),
                 color = "#DAD4D4") +
    autolayer(data_to_arima %>%
                filter(product == chosen_product),
              revenue,
              color = "#369093") +
    ggtitle("Expected revenue",
            subtitle = product_name) +
    xlab(NULL) +
    ylab("Revenue") +
    theme_minimal() +
    theme(text = element_text(colour = "#DAD4D4"),
          panel.grid = element_line(colour = "#423C3C"),
          panel.background = element_rect(fill = "#2D3741"),
          axis.text = element_text(colour = "#BCB1B1", 
                                   size = plot_font_size, 
                                   angle = 45, 
                                   hjust = 1),
          plot.background = element_rect(fill = "#2D3741", 
                                         color = "transparent"),
          legend.position = "none",
          legend.key.width = unit(2, "cm"),
          legend.box.margin = margin(t = 13),
          legend.background = element_rect(fill = "#2D3741"),
          legend.text = element_text(size = plot_font_size),
          legend.title = element_text(size = plot_font_size),
          plot.title = element_text(size = plot_font_size),
          axis.title = element_text(size = plot_font_size),
          axis.ticks = element_line(colour = "#BCB1B1"),
          panel.border = element_rect(fill = "transparent",
                                      colour = "#BCB1B1"),
          strip.text = element_text(colour = "#DAD4D4"),
          plot.subtitle = element_text(size = plot_font_size - 5)
    )
}

# Plots quantity vs time
plot_quantity_forecasts <- function(optimal_price_tibble,
                                    chosen_product,
                                    data_to_arima,
                                    plot_font_size){
  # Keep current product_name for later naming
  product_name <- chosen_product
  
  # Translate product_name to product_id
  chosen_product <- translate_input(chosen_product, data_to_arima)
  
  optimal_price_tibble %>% 
    autoplot(pred_quantity) +
    geom_point(color = "#DAD4D4") +
    autolayer(optimal_price_tibble, pred_quantity_normal) +
    geom_point(aes(y = pred_quantity_normal), color = "#369093") +
    geom_segment(aes(xend = yearmonth,
                     y = pred_quantity_normal,
                     yend = pred_quantity),
                 color = "#DAD4D4") +
    autolayer(data_to_arima %>% 
                filter(product == chosen_product), color = "#369093") +
    ggtitle("Expected sales quantity",
            subtitle = product_name) +
    xlab(NULL) +
    ylab("Quantity") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(colour = "#DAD4D4"),
          axis.text = element_text(colour = "#BCB1B1", 
                                   size = plot_font_size, 
                                   angle = 45, 
                                   hjust = 1),
          axis.ticks = element_line(colour = "#BCB1B1"),
          axis.title = element_text(size = plot_font_size),
          plot.title = element_text(size = plot_font_size),
          plot.background = element_rect(fill = "#2D3741", color = "transparent"),
          
          
          panel.grid = element_line(colour = "#423C3C"),
          panel.border = element_rect(fill = "transparent", colour = "#BCB1B1"),
          panel.background = element_rect(fill = "#2D3741"),
          
          strip.text = element_text(colour = "#DAD4D4"),
          plot.subtitle = element_text(size = plot_font_size - 5)
    )
}

# Plots the revenue effect due to price optimization
plot_revenue_price <- function(all_prices,
                               chosen_product,
                               data_to_arima,
                               plot_font_size){
  
  # Keep current product_name for later naming
  product_name <- chosen_product
  
  # Translate product_name to product_id
  chosen_product <- translate_input(chosen_product, data_to_arima)
  
  all_prices %>% 
    select(-yearmonth) %>% 
    as.data.frame() %>% 
    ggplot() +
    geom_line(aes(y = pred_revenue, x = new_price), color = "#369093") +
    geom_point(aes(y = pred_revenue_normal, x = original_price), color = "#369093") +
    geom_point(data = . %>% 
                 slice(1) %>% 
                 as.data.frame(), 
               aes(y = pred_revenue, x = new_price), color = "#DAD4D4") +
    ggtitle("Expected price vs revenue",
            subtitle = product_name) +
    xlab("Product price") +
    ylab("Revenue") +
    theme_minimal() +
    theme(text = element_text(colour = "#DAD4D4"),
          panel.grid = element_line(colour = "#423C3C"),
          panel.background = element_rect(fill = "#2D3741"),
          axis.text = element_text(colour = "#BCB1B1", 
                                   size = plot_font_size, 
                                   angle = 45),
          plot.background = element_rect(fill = "#2D3741", 
                                         color = "transparent"),
          legend.position = "none",
          legend.background = element_rect(fill = "#2D3741"),
          legend.text = element_text(size = plot_font_size),
          legend.title = element_text(size = plot_font_size),
          plot.title = element_text(size = plot_font_size),
          axis.title = element_text(size = plot_font_size),
          axis.ticks = element_line(colour = "#BCB1B1"),
          panel.border = element_rect(fill = "transparent",
                                      colour = "#BCB1B1"),
          strip.text = element_text(colour = "#DAD4D4"),
          plot.subtitle = element_text(size = plot_font_size - 5)
    )
}

# Segment rules for RFM
create_segments <- function(rfm_result) {
  # segment boundaries
  recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
  recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
  frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
  frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
  monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
  monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
  
  rfm_segment(rfm_result, segment_names,
              recency_lower, recency_upper,
              frequency_lower, frequency_upper,
              monetary_lower, monetary_upper) 
}