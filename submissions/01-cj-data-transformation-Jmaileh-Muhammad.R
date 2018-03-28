library(tidyverse)
library(completejourney)
library(dplyr)

transaction_data <- transaction_data %>%
  select(quantity, sales_value, retail_disc, coupon_disc, coupon_match_disc, household_key, store_id, basket_id, product_id, week_no, day, trans_time)

# Question #1
transaction_data <- transaction_data%>%
  mutate(retail_disc = abs(retail_disc), coupon_disc = abs(coupon_disc), coupon_match_disc = abs(coupon_match_disc))

# Question #2
transaction_data <- transaction_data %>%
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity) %>%
  mutate(loyalty_price = (sales_value + coupon_match_disc) / quantity) %>%
  mutate(coupon_price  = (sales_value - coupon_disc) / quantity)

# Question #3
transaction_data %>%
  select(regular_price, everything()) %>%
  filter(regular_price <= 1)

# Of all of the products, 471,484 have a regular price less than or equal to a dollar.

transaction_data %>%
  select(loyalty_price, everything()) %>%
  filter(loyalty_price <= 1)

# Of all of the products, 720,928 have a loyalty price less than or equal to a dollar.

transaction_data %>%
  select(coupon_price, everything()) %>%
  filter(coupon_price <= 1)

# Of all of the products, 733,443 have a coupon price less than or equal to a dollar.

# Question 4

transaction_data %>%
  group_by(basket_id) %>%
  summarize(total_basket = sum(sales_value)) %>%
  ungroup(basket_id) %>%
  mutate(prop_over10 = mean(total_basket >10))

# The proportion of the basket that is over $10 is 65.4 %

# Question 5

transaction_data %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  arrange(desc(pct_loyalty_disc)) %>%
  group_by(store_id) %>%
  mutate(total_sales = sum(sales_value)) %>%
  filter(total_sales > 10000) %>%
  arrange(desc(pct_loyalty_disc)) %>%
  select(pct_loyalty_disc, everything())

transaction_data %>% 
  filter(
    is.finite(regular_price), 
    is.finite(loyalty_price), 
    regular_price > 0
  ) %>%
  mutate(pct_loyalty_disc=1-(loyalty_price/regular_price)) %>%
  group_by(store_id)%>% 
  summarise(total_sales=sum(sales_value),  avg_pct_loyalty_disc = mean(pct_loyalty_disc)) %>% 
  filter(total_sales>10000) %>%
  arrange(desc(avg_pct_loyalty_disc))

#Store 341 has the highest pct of loyalty discount per product at .188 percent of the price
