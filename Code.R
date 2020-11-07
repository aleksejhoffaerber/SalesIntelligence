##### Group 1 - BAN 400 ####

###############################################################################
################### Sales Intelligence ########################################
###############################################################################

rm(list = ls())

##### Libraries ####
library(tidyverse)
library(readr)
library(Metrics)
library(magrittr)
library(lubridate)
library(zoo)


#### Data Cleaning and Exploration ####

# Load the data (generic)
files <- list.files("Data/",pattern = "*.csv") # Files to load

# Data Table
# Cleaning & Joining
raw_prices <- tibble(Invoice = character(),
                     StockCode = character(),
                     Description = character(),
                     Quantity = numeric(),
                     InvoiceDate = as.Date(as.character()),
                     Date = as.Date(as.character()),
                     Price = as.numeric(),
                     `Customer ID` = as.numeric(),
                     Country = as.character())

for(i in seq_along(files)) {
  read_csv(paste("Data/",files[i],sep = "")) %>%
    separate(.,InvoiceDate,c("InvoiceDate","Time"),sep = " ") %>% # Separates Date from Time
    mutate(InvoiceDate = as.Date(InvoiceDate),
           Date = as.yearmon(paste(year(InvoiceDate),month(InvoiceDate)),"%Y %m")) %>% # Year & Month
    filter(is.na(`Customer ID`)==F & Price > 0 & Quantity > 0) %>% # Removes errors / non-commercial actions
    filter(grepl("^[[:alpha:]]",StockCode)==F) %>% # Removes non - commercial actions (missing serial nr.)
    select(-Time) %>%
    rbind(raw_prices,.) -> raw_prices
}

