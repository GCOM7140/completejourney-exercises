#' ---
#' title: "Data Visualization Solutions"
#' output: github_document
#' ---
library(tidyverse)
library(completejourney)
library(lubridate)

#' Question 1

ggplot(data = transactions) + 
  geom_histogram(mapping = aes(x = quantity))

ggplot(data = transactions %>% filter(quantity <= 10)) + 
  geom_histogram(mapping = aes(x = quantity))
#' the data is extremely skewed towards quantities close to zero (there are
#' outliers far to the right)

#' Question 2
transactions %>% 
  mutate(date = date(transaction_timestamp)) %>% 
  group_by(date) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = date, y = total_sales_value))

#' total sales were extremely high right before January (towards the end of
#' december) likely because it was the holidays

#' Question 3

transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% 
  mutate(brand  = fct_explicit_na(brand)) %>% 
  filter(brand != "(Missing)")

transactions_products %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(mapping = aes(x = brand, y = total_sales_value)) + 
  geom_bar(stat = "identity")

#' Question 4

transactions_products %>%
  filter(product_category %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(
    mapping  = aes(x = product_category, y = total_sales_value, fill = brand)
  ) + 
  geom_col(position = "fill")

#' Question 5
 
transactions_products %>% 
  filter(product_category == "PNT BTR/JELLY/JAMS") %>% 
  group_by(package_size) %>% 
  summarize(count = n()) %>% 
  ggplot() + 
  geom_bar(
    mapping = aes(x = package_size %>% fct_reorder(count), y = count), 
    stat    = "identity"
  ) +
  coord_flip()

#' the two most popular package sizes are 18 and 32 Ounce in that order