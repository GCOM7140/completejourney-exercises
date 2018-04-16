#' ---
#' title: "Answers to the CJ 04 Data Wrangling Exercises"
#' author: Robert Papel
#' date: April 16, 2018
#' output: github_document
#' ---

library(tidyverse)
library(completejourney)

#' **Exercise 1**: What percent of households that received the retailer's
#' weekly mailer redeemed at least one coupon?

left_join(campaign_table %>% count(household_key), 
          coupon_redempt %>% count(household_key), by = 'household_key') %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))

#'Answer: 27.4% received a coupon

#' **Exercise 2**: How many households did not redeem a coupon?

left_join(campaign_table %>% count(household_key),
          coupon_redempt %>% count(household_key), by = 'household_key') %>% 
  summarize(redemption_rate = sum(is.na(n.y)))

#'Answer: 1150 coupons not redeemed.

#' **Exercise 3**: What percent of coupons promoted in the retailer's weekly
#' mailer got redeemed at least once?

left_join(coupon %>% count(coupon_upc),
          coupon_redempt %>% count(coupon_upc), by = 'coupon_upc') %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))

#'Answer: 49% got redeemed

#' **Exercise 4**: Using the transaction_data and product datasets, determine
#' which product category (i.e., sub_commodity_desc) grew the most in terms of
#' revenue for the retailer in the second half of the study period (i.e.,
#' between week_no == 52 and week_no == 102). Only consider product categories
#' that had over $100 in revenue in week 52, and calculate revenue growth as a
#' percentage of category revenue in week 52.

left_join(transaction_data, product) %>%
  group_by(sub_commodity_desc, week_no) %>%
  summarize(sales_value_category = sum(sales_value, na.rm = TRUE)) %>%
  filter(week_no == 52 | week_no == 102) %>%
  mutate(revenue_growth = ifelse(lag(sales_value_category) >= 100,
                                 (sales_value_category - 
                                    lag(sales_value_category)) /
                                   lag(sales_value_category), NA)) %>%
  arrange(desc(revenue_growth))

#' Answer: 85.2% growth