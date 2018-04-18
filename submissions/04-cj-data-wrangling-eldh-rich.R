#----------------------------------------------------------------
#Rich Eldh
#Professor Boichek
#Customer Analytics
#Assignment #4
#Due Date: 17 April 2018
#----------------------------------------------------------------

#----------------------------------------------------------------
#Complete Journey Exercises
#----------------------------------------------------------------

library(tidyverse)
library(completejourney)

#----------------------------------------------------------------
#Question 1
#----------------------------------------------------------------

left_join(
  campaign_table %>% 
  count(household_key), coupon_redempt %>%
  count(household_key), by = 'household_key') %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))

#27.4% of the 1584 households that were mailed coupons redeemed a coupon.

#----------------------------------------------------------------
#Question 2
#----------------------------------------------------------------

left_join(
  campaign_table %>% 
  count(household_key), coupon_redempt %>%
  count(household_key), by = 'household_key') %>% 
  summarize(redemption_rate = sum(is.na(n.y)))

#1150 of those 1584 households did not redeem a coupon.

#----------------------------------------------------------------
#Question 3
#----------------------------------------------------------------

left_join(
  coupon %>%
  count(coupon_upc), coupon_redempt %>% 
  count(coupon_upc), by = 'coupon_upc') %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))

#49% of the 1135 coupons were redeemed at least once.

#----------------------------------------------------------------
#Question 4
#----------------------------------------------------------------

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


#The select beef product category grew the most, by 85.2% of revenue. 