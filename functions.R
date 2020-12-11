# Make forecasts for different prices
get_forecasts <- function(chosen_product){
  new_data <- data_to_arima %>% 
    filter(product %in% chosen_product) %>% 
    # Get the last observation
    group_by(product) %>% 
    slice(n()) %>%
    mutate(yearmonth = yearmonth + 1, # TODO depending on duration
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
               filter(product %in% product) %>% 
               # Get the last observation
               group_by(product) %>% 
               slice(n()) %>%
               mutate(yearmonth = yearmonth + 1)) %>% 
    mutate(pred_quantity_normal = ifelse(.mean < 0, 0, .mean)) %>% 
    as_tsibble() %>% 
    select(product, yearmonth, pred_quantity_normal) %>% 
    as_tibble() %>% 
    inner_join(forecasts) %>% 
    mutate(pred_revenue_normal = pred_quantity_normal * original_price)
}

# Obtain optimal prices to maximize sales
get_optimal_forecast <- function(forecasts){
  # Get optimal prices by maximizing revenue
  forecasts %>% 
    slice(1) %>% 
    as_tsibble(key = "product")
}

plot_revenue_forecasts <- function(optimal_forecast, chosen_product){
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
    ggtitle("Effect of price optimization on expected revenue",
            subtitle = "TODO product name") + # TODO
    xlab(NULL) +
    ylab("Quantity") +
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

# FIXME
plot_quantity_forecasts <- function(optimal_price_tibble, product){
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
                filter(product %in% product), color = "#369093") +
    facet_wrap(~product, scales = "free") +
    ggtitle("Effect of price optimization on expected sales quantity") +
    xlab(NULL) +
    ylab("Revenue") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(colour = "#DAD4D4"),
          panel.grid = element_line(colour = "#423C3C"),
          axis.ticks = element_line(colour = "#BCB1B1"),
          axis.text = element_text(colour = "#BCB1B1"),
          plot.background = element_rect(fill = "#2D3741", color = "transparent"),
          panel.border = element_rect(fill = "transparent", colour = "#BCB1B1"),
          strip.text = element_text(colour = "#DAD4D4"))
}
