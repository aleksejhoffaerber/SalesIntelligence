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
  inner_join(data_monthly)
