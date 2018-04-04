#' ---
#' title: "CJ Exercise #2"
#' author: Burhan Khan
#' date: March 30, 2018
#' output: github_document
#' ---

knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(completejourney)
library(ggplot2)
library(scales)

#Question 1
ggplot(transaction_data, aes(quantity)) +
  geom_histogram() +
  scale_y_continuous(labels = comma)

#The histogram is very high at the "0" value and has almost no other values showing up.

#Question 2
transaction_data %>%
  group_by(day) %>%
  ggplot(aes(day, sales_value)) +
  geom_line()

#This looks kind of like a bar chart


#Question 3
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')

my_transaction_data %>%
  group_by(brand) %>%
  summarize(sales = sum(sales_value)) %>%
  ggplot(aes(brand, sales)) +
  geom_bar(stat = "identity", fill = "lightblue", alpha = .8) +
  theme_bw() +
  scale_y_continuous(labels = comma)

#Question 4
my_transaction_data %>%
  filter(commodity_desc %in% c('SOFT DRINKS', 'CHEESE')) %>%
  group_by(commodity_desc, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot() + 
  geom_bar(
    mapping  = aes(x = commodity_desc, y = total_sales_value, fill = brand), 
    stat     = 'identity', 
    position = 'fill')

#Question 5
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product))))

ggplot(pb_and_j_data) + 
  geom_bar(aes(x = product_size))

pb_and_j_data %>% 
  count(product_size) %>% 
  arrange(-n)
