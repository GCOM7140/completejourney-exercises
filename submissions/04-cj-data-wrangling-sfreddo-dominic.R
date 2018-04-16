#' ---
#' title: "CJ Exercise #4"
#' author: Dominic Sfreddo
#' date: April 15, 2018
#' output: github_document
#' ---

knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(completejourney)

#Exercise 1

hh_coupon_redemptions <- coupon_redempt %>% 
  count(household_key)
hh_campaign_households <- campaign_table %>% 
  distinct(household_key)
hh_redemptions <- left_join(hh_campaign_households, hh_coupon_redemptions, by = 'household_key')
mean(!is.na(hh_redemptions$n))

#27.4% of the households that received a mailing actually redeemed a coupon.

#Exercise 2

anti_join(hh_campaign_households, hh_coupon_redemptions, by = 'household_key') %>% 
  nrow()

#1150 households did not redeem a coupon

#Exercise 3

mail_coupons <- coupon %>% 
  distinct(coupon_upc)
mail_coupons_redeemed <- left_join(mail_coupons, coupon_redempt, by = 'coupon_upc')
mean(!is.na(mail_coupons_redeemed$household_key))

#80% of coupons in the mailer got redeemed at least once.

#Exercise 4

my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
my_transaction_data_named <- my_transaction_data %>% 
  group_by(sub_commodity_desc, week_no) %>% 
  summarise(total_sales_value = sum(sales_value, na.rm = TRUE)) %>% 
  filter(week_no %in% c(52, 102)) %>% 
  filter(total_sales_value >= 100) %>% 
  spread(key = week_no, value = total_sales_value)
  colnames(my_transaction_data_named) <- c("sub_commodity_desc", "week52", "week102")

my_transaction_data_named %>% 
  summarise(revenue_growth = (week102 - week52) / week52) %>% 
  arrange(desc(revenue_growth))

#select beef had the most growth.