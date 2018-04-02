# Richard Potter
# Data Visualization CJ Homework Assignment
# Professor Boichuk
# 3 April 2018

# Initial Packages --------------------------------------------------------
library(tidyverse)
library(completejourney)

# Question 1 --------------------------------------------------------------
transaction_data <- transaction_data %>%
  select(
    quantity,
    sales_value,
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id,
    week_no, day, trans_time
  )

transaction_data %>% 
  ggplot() +
  geom_histogram(aes(quantity))
# This visualization includes many large numbers, toward 100,000, which means
# that almost all of the basket quantities are grouped around 0, rendering the
# information in the graph almost useless. In the end, I would adjust the bin
# width if I wanted to find meaningful information.


# Question 2 --------------------------------------------------------------
transaction_data %>% 
  group_by(day, sales_value) %>% 
  summarise(totalsales = sum(sales_value)) %>% 
  ggplot() +
  geom_line(aes(day, totalsales))

# There is far too much information in order for the graph to be meaningful,
# the lines are causing things to be much more frantic than for the size of the
# graph. I would either expand the size, find the sum by week, or run a 
# geom_smooth.

# Question 3 --------------------------------------------------------------
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
my_transaction_data %>% 
  group_by(brand) %>%
  summarise(totalsalesbrand = sum(sales_value)) %>% 
  ggplot(aes(brand)) +
  geom_bar(aes(weight = totalsalesbrand, fill = brand))

# Question 4 --------------------------------------------------------------
my_transaction_data %>% 
  filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>% 
  group_by(commodity_desc, brand) %>% 
  summarise(totalsalesproduct = sum(sales_value)) %>% 
  ggplot() + 
  geom_bar(aes(commodity_desc, weight = totalsalesproduct, fill = brand), position = 'fill')

# Question 5 --------------------------------------------------------------
pb_and_j_data <- my_transaction_data %>%
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)',
                                                '\\1', 
                                                curr_size_of_product)))) 
pb_and_j_data %>% 
  ggplot() + 
  geom_bar(aes(product_size))
# 18 and 32 ounces are the most popular product sizes.
