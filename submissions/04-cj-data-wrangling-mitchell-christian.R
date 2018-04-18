# Complete Journey - Howework 4 - Data Wrangling
  # Christian Mitchell

library(tidyverse)
library(completejourney)


# Problem 1

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), 
  by = 'household_key'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))
  # 27.4%

# Problem 2

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), 
  by = 'household_key'
) %>% 
  summarize(redemption_rate = mean(is.na(n.y)))

  # 72.6%

# Problem 3

left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc),
  by = "coupon_upc"
) %>% 
  summarize(redemtion_rate = mean(!is.na(n.y)))

  # 49%

# Problem 4

left_join(
  transaction_data, product
) %>% 
  group_by(sub_commodity_desc, week_no) %>% 
  summarize(category_sales = sum(sales_value, na.rm = TRUE)) %>% 
  filter(week_no == 52 | week_no == 102) %>% 
  mutate(
    revenue_growth = ifelse(lag(category_sales) >= 100, 
                            (category_sales - lag(category_sales) / 
                               lag(category_sales)), NA)
  ) %>% 
    arrange(desc(revenue_growth))
  