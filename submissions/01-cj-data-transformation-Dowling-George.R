##### HW1 Dowling George ####

library(completejourney)
library(tidyverse)

transaction_data <- transaction_data %>% 
  select( quantity, sales_value, retail_disc, coupon_disc, coupon_match_disc, household_key, store_id, basket_id, product_id, week_no, day, trans_time )

####Question 1 ####
transaction_data <- transaction_data %>%
    mutate(
            retail_disc = abs(retail_disc),
            coupon_disc = abs(coupon_disc),
            coupon_match_disc = abs(coupon_match_disc)
                          )


#### Question 2 ####
transaction_data <- transaction_data %>%
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity) %>%
  mutate(loyalty_price =(sales_value + coupon_match_disc) / quantity) %>%
  mutate(coupon_price  = (sales_value - coupon_disc) / quantity)


#### Question 3 ####

# Price less than $1 #

transaction_data %>%
  filter(regular_price <= 1 ) %>%
  select(product_id) %>%
  n_distinct()
### 12442 ###

# Count of loyalty and coupon prices #

transaction_data %>%
  filter(loyalty_price <= 1) %>%
  select(product_id) %>%
  n_distinct()
### 20113 ###

transaction_data %>%
  filter(coupon_price <= 1) %>%
  select(product_id) %>%
  n_distinct()
### 22273 ###


#### Question 4 ####

transaction_data %>%
  group_by(basket_id) %>%
  summarise(basket_price = sum(sales_value)) %>%
  ungroup() %>%
  summarise(prop_over_10 = mean(basket_price > 10))
### 65.4% ###


#### Question 5 ####

transaction_data %>%
  filter(
    is.finite(regular_price),
    is.finite(loyalty_price),
    regular_price > 0
  ) %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  group_by(store_id) %>%
  summarise(
    total_sales = sum(sales_value),
    avg_loyalty_disc = mean(pct_loyalty_disc)
  ) %>%
  filter(total_sales > 10000) %>%
  arrange(desc(avg_loyalty_disc))

### store 341 ###