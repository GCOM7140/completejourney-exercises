#'-----------------
#' Title:   Customer journey data wrangling exercises
#' Author:  Lani Hannah
#' Date:    April 17, 2018
#'-----------------

library(tidyverse)
library(completejourney)


#' Question 1: What percent of households that received the retailer's weekly mailer redeemed at least one coupon?

hh_coupon_count <- coupon_redempt %>% 
  count(household_key)
hh_campaign_participants <- campaign_table %>% 
  distinct(household_key)
hh_redeemed_mailer <- left_join(hh_campaign_participants, hh_coupon_count, by = "household_key")
mean(!is.na(hh_redemption_check$n))

#' Question 2: How many households did not redeem a coupon? 
(hh_no_redeem <- anti_join(hh_campaign_participants, hh_coupon_count, by = "household_key") %>% 
  nrow())

#' Question 3: What percent of coupons promoted in the retailer's weekly mailer got redeemed at least once?
left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by = 'coupon_upc'
          ) %>% 
  summarise(redemption_rate = mean(!is.na(n.y)))

#' Question 4: Using the transaction_data and product datasets, determine which product category (i.e., sub_commodity_desc) grew the most in terms of revenue for the retailer in the second half of the study period (i.e., between week_no == 52 and week_no == 102). Only consider product categories that had over $100 in revenue in week 52, and calculate revenue growth as a percentage of category revenue in week 52.

left_join(transaction_data, product, by = "product_id") %>% 
  group_by(sub_commodity_desc, week_no) %>%  
  filter(week_no == 52 | week_no == 102) %>%
  summarize(sales_value_category = sum(sales_value, na.rm = TRUE)) %>%
  mutate(
    revenue_growth = ifelse(lag(sales_value_category) >= 100,
                            (sales_value_category - lag(sales_value_category)) /
                              lag(sales_value_category), NA)
  ) %>%
  arrange(desc(revenue_growth))
  
