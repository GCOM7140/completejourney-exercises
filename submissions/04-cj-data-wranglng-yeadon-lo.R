##################################################
## Project: Customer Journey Data Wrangling
## Script purpose: Homework Submision
## Date: 17 Apirl 2018
## Author: Lo Yeadon
##################################################

library(tidyverse)
library(completejourney)

#Excercise 1: What percent of households that received the retailer's weekly mailer redeemed at least one coupon?

#Answer 1: 434 (27.4%) redeemed a coupon.

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))


#Excercise 2:How many households did not redeem a coupon?

#Answer 2: 1,150 households did not redeem a coupon.

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = sum(is.na(n.y)))


#Exercise 3:What percent of coupons promoted in the retailer's weekly mailer got redeemed at least once?

#Answer 3: 556 (49%) coupons were redeemed at least once.

left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by = 'coupon_upc'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y))

            
#Excercse 4: Using the transaction_data and product datasets, determine which product category (i.e., sub_commodity_desc) grew the most in terms of revenue for the retailer in the second half of the study period (i.e., between week_no == 52 and week_no == 102). Only consider product categories that had over $100 in revenue in week 52, and calculate revenue growth as a percentage of category revenue in week 52.


#Answer 4: The select beef category grew the most among all product categories (85.2% in the second half of the study period)


left_join(transaction_data, product) %>%
  group_by(sub_commodity_desc, week_no) %>%
  summarize(sales_value_category = sum(sales_value, na.rm = TRUE)) %>%
  filter(week_no == 52 | week_no == 102) %>%
  mutate(
    revenue_growth = ifelse(lag(sales_value_category) >= 100,
                            (sales_value_category - lag(sales_value_category)) /
                              lag(sales_value_category), NA)
  ) %>%
  arrange(desc(revenue_growth))