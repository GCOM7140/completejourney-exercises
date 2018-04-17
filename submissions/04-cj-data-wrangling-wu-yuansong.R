#Data Wrangling Exercise - completejourney
#Yuansong(Kevin) Wu

library(tidyverse)
library(completejourney)


# Exercise 1 
left_join(
  campaign_table %>% count(household_key), 
  coupon_redempt %>% count(household_key), 
  by = 'household_key') %>%
  summarize(ratio = mean(!is.na(n.y)))

# Exercise 2
left_join(
  campaign_table %>% count(household_key), 
  coupon_redempt %>% count(household_key), 
  by = 'household_key') %>%
  summarize(ratio = sum(is.na(n.y)))  

# Exercise 3
left_join(
  coupon %>% count (coupon_upc),
  coupon_redempt %>% count (coupon_upc),
  by = 'coupon_upc'
) %>%
  summarize(ratio = mean(!is.na(n.y)))


# Exercise 4 
left_join(transaction_data, product, by = 'product_id') %>%
  group_by(sub_commodity_desc, week_no) %>%
  mutate (total_sales = sum (sales_value)) %>%
  filter (week_no %in% c('52', '102')) %>%
  filter (total_sales >= 100) %>%
  mutate (revenue_growth = (total_sales - lag(total_sales)) / lag(total_sales)) %>%
  arrange (desc(revenue_growth))
  
  
  