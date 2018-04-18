library(tidyverse)
library(completejourney)

#1
left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))

#27.4 redeemed a coupon

#2
left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = sum(is.na(n.y)))
#1150 households did not redeem a coupn

#3
left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by = 'coupon_upc'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))

#49% of them were redeemed

#4
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

#the select beef category grew the most among all product categories that period