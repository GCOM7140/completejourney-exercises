# Data-Wrangling Exercises

library(tidyverse)
library(completejourney)
library(lubridate)

# Question 1
left_join(
  campaigns %>% count(household_id, name = "n_recipients"),
  coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = "household_id"
) %>% 
  summarize(redemption_rate = mean(!is.na(n_redemptions)))

# 26.3%

# Question 2
left_join(
  campaigns %>% count(household_id, name = "n_recipients"),
  coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = "household_id"
) %>% 
  summarize(redemption_rate = sum(is.na(n_redemptions)))

# 1,149

# Question 3
left_join(
  coupons %>% count(coupon_upc, name = "n_products", sort = TRUE),
  coupon_redemptions %>% count(coupon_upc, name = "n_redemptions"), 
  by = "coupon_upc") %>% 
  summarize(redemption_rate = sum(!is.na(n_redemptions)))

# 491

# Question 4
transactions %>% 
  left_join(products, by = "product_id") %>%
  mutate(month = month(transaction_timestamp, label = TRUE)) %>%
  group_by(product_category, month) %>%
  summarize(spend_tot = sum(sales_value, na.rm = TRUE)) %>%
  filter(month == "Jan" | month == "Mar") %>%
  group_by(product_category) %>% 
  mutate(
    spend_growth_pct = (spend_tot - lag(spend_tot)) / lag(spend_tot) * 100,
    spend_jan = first(spend_tot)
  ) %>% 
  filter(spend_jan >= 1500) %>% 
  arrange(desc(spend_growth_pct)) %>% 
  select(product_category, spend_growth_pct) %>% 
  head(5)

# 53.4% spending growth increase in infant formula, 29.8% in frozen seafood, 23.3% in packaged candy, 19.5% in oral hygiene, 13.5% in domestic wine