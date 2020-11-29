# Make forecasts and obtain optimal prices to maximize sales
get_optimal_prices <- function(products){
  new_data <- data_to_arima %>% 
    filter(product %in% products) %>% 
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
  
  # Make forecasts for different prices
  forecasts <- future_map(1:nrow(new_data),
                          ~models %>% 
                            filter(product == new_data %>% 
                                     slice(.x) %>% 
                                     pull(product)) %>% 
                            forecast(new_data %>% 
                                       slice(.x) %>% 
                                       as_tsibble(key = product, 
                                                  index = yearmonth)) %>% 
                            as_tibble()) %>% 
    reduce(bind_rows)
  
  # Calculate optimal prices by maximizing revenue
  forecasts %>% 
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
    arrange(-pred_revenue) %>% 
    slice(1) %>% 
    as_tsibble(key = "product")
}

plot_quantity_forecasts <- function(optimal_price_tibble, products){
  optimal_price_tibble %>% 
    autoplot(pred_quantity) +
    geom_point(color = "black") +
    autolayer(optimal_price_tibble, original_quantity) +
    geom_point(aes(y = original_quantity)) +
    geom_segment(aes(xend = yearmonth,
                     y = original_quantity,
                     yend = pred_quantity),
                 color = "black") +
    autolayer(data_to_arima %>% 
                filter(product %in% products)) +
    facet_wrap(~product, scales = "free") +
    ggtitle("Effect of price optimization on expected sales quantity") +
    xlab(NULL) +
    ylab("Quantity") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45,
                                     hjust = 1))
}
