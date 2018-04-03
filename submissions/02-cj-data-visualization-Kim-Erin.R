library(tidyverse)
library(completejourney)
library(ggplot2)
library(dplyr)

knitr::opts_chunk$set(echo = TRUE)

# 1 ----------------------------------------------------------------------

ggplot(data = transaction_data) +
  geom_histogram(mapping = aes(x = quantity))


# 2 -----------------------------------------------------------------------

transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales = sum(sales_value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(mapping = aes(x = day, y = total_sales), stat = 'identity')


# 3 -----------------------------------------------------------------------

my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
my_transaction_data %>% 
  group_by(brand) %>% 
  summarize(total_sales_value = sum(sales_value)) %>% 
  ggplot() +
  geom_bar(
    mapping = aes(x = brand, y = total_sales_value)
  )


# 4 -----------------------------------------------------------------------

my_transaction_data %>% 
  filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>% 
  group_by(commodity_desc, brand) %>% 
  summarize(total_product_salesv = sum(sales_value)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(mapping = aes(x = commodity_desc, y = total_product_salesv, fill = brand), stat = 'identity', position = 'fill')


# 5 -----------------------------------------------------------------------

pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )  
pb_and_j_data %>% 
  group_by(product_size) %>% 
  summarize(total_sales = sum(sales_value)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = product_size, y = total_sales))
