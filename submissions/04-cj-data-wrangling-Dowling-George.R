#Dowling George HW 4
#Customer Journey

library(tidyverse)
library(completejourney)
library(ggplot2)

# Problem 1

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key') %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))


#Problem 2

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key') %>% 
  summarize(redemption_rate = sum(is.na(n.y)))


#Problem 3

left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by = 'coupon_upc') %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))


# Problem 4

left_join(transaction_data, product) %>%
  group_by(sub_commodity_desc, week_no) %>%
  summarize(sales_value_category = sum(sales_value, na.rm = TRUE)) %>%
  filter(week_no == 52 | week_no == 102) %>%
  mutate(
    revenue_growth = ifelse(lag(sales_value_category) >= 100,
                            (sales_value_category - lag(sales_value_category)) /
                              lag(sales_value_category), NA)) %>%
  arrange(desc(revenue_growth))
