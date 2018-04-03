# Exercise 2 -  Data Visualization - Complete Journey
  # Christian Mitchell

library(tidyverse)
library(completejourney)

# Question 1

ggplot(transaction_data) +
  geom_histogram(aes(quantity)) # This looks like a single bar, all close to 0

# Question 2

transaction_data %>% 
  group_by(day) %>% 
  summarize(day_sales = sum(sales_value, na.rm = TRUE)) %>% 
  ggplot() + 
  geom_line(aes(day, day_sales)) # There appear to be days where the sales value drops to 0, so could be holidays where Kroger was closed

# Question 3

my_transaction_data <- left_join(
  transaction_data, product, by = 'product_id')

my_transaction_data %>% 
  group_by(brand) %>% 
  summarize(brand_sales = sum(sales_value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_bar(aes(brand, brand_sales),
           stat = 'identity')

# Question 4

my_transaction_data %>% 
  filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>% 
  group_by(commodity_desc, brand) %>% 
  summarize(total_sales = sum(sales_value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_bar(aes(commodity_desc, total_sales, fill = brand),
           stat = 'identity',
           position = 'fill')

# Question 5

b_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )

ggplot(b_and_j_data) +
  geom_bar(aes(product_size)) # 18 ounces and 32 ounces are the most popular
