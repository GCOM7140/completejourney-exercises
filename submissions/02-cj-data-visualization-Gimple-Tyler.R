library(tidyverse)
library(completejourney)
library(ggplot2)
library(dplyr)
#Question 1#
ggplot(data = transaction_data, mapping = aes(quantity)) +
  geom_histogram()
#Question 2#
transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = day, y = total_sales_value))

#Question3#
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
my_transaction_data %>% 
  group_by(brand) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = FALSE)) %>%
  ggplot() +
  geom_bar(aes(brand, total_sales_value, fill = brand), stat = "identity")

#Question 4#
my_transaction_data %>% 
  filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>% 
  group_by(commodity_desc, brand) %>% 
  summarize(total_product_salesv = sum(sales_value)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(mapping = aes(x = commodity_desc, y = total_product_salesv, fill = brand), stat = 'identity', position = 'fill')

#Question5#
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )

ggplot(pb_and_j_data) + 
  geom_bar(aes(x = product_size))