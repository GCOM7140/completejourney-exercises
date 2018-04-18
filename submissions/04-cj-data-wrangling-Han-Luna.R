#' ---
#' title: "Answers to the CJ EDA Exercise"
#' author: Luna Han
#' date: April 16, 2018
#' output: github_document
#' ---

library(tidyverse)
library(completejourney)

#Exercise 1 What percent of households that received the retailer's weekly mailer redeemed at least one coupon?
?coupon_redempt
?campaign_table

  left_join(
    campaign_table %>% count(household_key), 
    coupon_redempt %>% count(household_key), 
    by = 'household_key') %>%
  mutate(ratio = mean(!is.na(n.y)))
  
# Exercise 2 How many households did not redeem a coupon?
  left_join(
    campaign_table %>% count(household_key), 
    coupon_redempt %>% count(household_key), 
    by = 'household_key') %>%
    mutate(ratio = mean(is.na(n.y)))  
  
# Exercise 3 What percent of coupons promoted in the retailer's weekly mailer got redeemed at least once?
  left_join(
    coupon %>% count (coupon_upc),
    coupon_redempt %>% count (coupon_upc),
    by = 'coupon_upc'
  ) %>%
    mutate(ratio = mean(!is.na(n.y)))

  
# Exercise 4 Using the transaction_data and product datasets, determine which product category (i.e., sub_commodity_desc) grew the most in terms of revenue for the retailer in the second half of the study period (i.e., between week_no == 52 and week_no == 102). Only consider product categories that had over $100 in revenue in week 52, and calculate revenue growth as a percentage of category revenue in week 52.
transaction_data
product
?lag
left_join(transaction_data, product, by = 'product_id') %>%
  group_by(sub_commodity_desc, week_no) %>%
  mutate (total_sales = sum (sales_value)) %>%
  filter (week_no %in% c('52', '102')) %>%
  filter (total_sales >= 100) %>%
  mutate (revenue_growth = (total_sales - lag(total_sales)) / lag(total_sales)) %>%
  arrange (desc(revenue_growth)) %>%
  

  