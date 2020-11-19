library(fable)
library(purrr)
library(scales)
library(tsibble)
library(Metrics)
library(stringr)
library(lubridate)
library(tidyverse)
library(data.table)
library(strucchange)

rm(list = ls())

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
            price_mean = weighted.mean(Price,Quantity, na.rm = TRUE),
            revenue = sum(Price * Quantity),
            n_receipts = n()) %>% 
  group_by(product) %>% 
  # Keep only products with two years of observations or more
  mutate(n_months = n()) %>% 
  ungroup() %>%
  filter(n_months >= 24) %>%
  arrange(product,yearmonth)

#### Test for Structural Change ####
# Remove series with significant brakes. 

# The functions performs a Cumulative sum test for parameter stability
# and returns the p - vale

structual_test <- function(prod) {
  
  series <- data_monthly %>% 
    filter(product==prod) %>%
    select(price_mean)
  
  price.cum <- efp(series$price_mean ~ 1,
                   type = "Rec-CUSUM")
  test <- sctest(price.cum)
  
  p <- cbind(prod,test$p) %>%
    as.data.frame()
  
  return(p)
}

# Loop though all products
p_values <- tibble()
for (id in unique(data_monthly$product)) {
  structual_test(id) %>%
    rbind(p_values,.) -> p_values
}

# Remove all products with significant structural brakes
p_values %>%
  rename(product = prod,
         p_value = V2) %>%
  filter(is.na(p_value)==F) %>%
  filter(p_value > 0.05) %>%
  select(product) %>%
  merge(.,data_monthly) -> data_monthly
