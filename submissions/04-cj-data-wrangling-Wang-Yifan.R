knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(completejourney)


# Question 1

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))

# Of all the households that were mailed coupons as part of a campaign, 27.4% redeemed a coupon.


# Question 2

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = sum(is.na(n.y)))

# 1,150 households did not redeem a coupon.


# Question 3

left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by = 'coupon_upc'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))

# Of the 1,135 coupons promoted in the retailer's weekly mailer, 49% coupons were redeemed at least once.


# Question 4

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

# Having grown 85.2% in the second half of the study period, the select beef category grew the most among all product categories.