library(completejourney)
library(tidyverse)
library(lubridate)

# Question 1
left_join(
  campaigns %>%
    count(household_id, name = "received"),
  coupon_redemptions %>%
    count(household_id, name = "redeemed"),
  by = "household_id"
) %>%
  summarize(rate = mean(!is.na(redeemed)))
# 26.3 of households redeemed coupons

# Question 2
left_join(
  campaigns %>%
    count(household_id, name = "received"),
  coupon_redemptions %>%
    count(household_id, name = "redeemed"),
  by = "household_id"
) %>%
  summarize(unredeemed = sum(is.na(redeemed)))
# 1149 households received and did not redeem coupons

# Question 3
left_join(
  coupons %>%
    count(coupon_upc, name = "n_coupons"),
  coupon_redemptions %>%
    count(coupon_upc, name = "n_redeemed"),
  by = "coupon_upc"
) %>%
  summarize(unredeemed = mean(!is.na(n_redeemed)))
# 50.1% of coupons promoted were redeemed at least once

# Question 4
left_join(transactions, products, by = "product_id") %>%
  mutate(month = month(transaction_timestamp)) %>%
  group_by(product_category, month) %>%
  summarize(total_spend = sum(sales_value)) %>%
  filter(month %in% c(1,3)) %>%
  mutate(spend_growth_pct = (total_spend - lag(total_spend))/lag(total_spend),
         jan_spend = first(total_spend)) %>%
  filter(jan_spend > 1500) %>%
  arrange(desc(spend_growth_pct))
