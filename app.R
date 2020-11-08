library(fable)
library(purrr)
library(scales)
library(tsibble)
library(Metrics)
library(stringr)
library(lubridate)
library(tidyverse)
library(data.table)

# Data reading and cleaning -----------------------------------------------

# Get paths of all input files and load them
files <- paste0("Data/", list.files("Data/", pattern = "*.csv"))

data_by_invoice <- map(files,
                       ~fread(.x) %>% 
                         as_tibble()) %>% 
  reduce(bind_rows)


data_monthly <- data_by_invoice %>%
  mutate(yearmonth = yearmonth(as.Date(InvoiceDate)),
         # Combine same products with different colors
         product = str_sub(StockCode, 1, 5)) %>% 
  filter(Quantity > 0,
         Price > 0,
         # Remove those without numbers in product ID
         !str_detect(product, "^[[:alpha:]]")) %>% 
  group_by(yearmonth, Country, product) %>%
  # Calculate amount of sales, average prices, revenue and receipt amount
  summarise(quantity_sum = sum(Quantity, na.rm = TRUE),
            price_mean = mean(Price, na.rm = TRUE),
            revenue = sum(Price * Quantity),
            n_receipts = n()) %>% 
  group_by(Country, product) %>% 
  # Keep only products with two years of observations or more
  mutate(n_months = n()) %>% 
  filter(n_months >= 24)
