# Richard Potter
# Data Transformation Customer Journey Homework Assignment
# Professor Boichuk
# 27 March 2018

library(tidyverse)
library(completejourney)
transaction_data <- transaction_data %>%
  select(
    quantity,
    sales_value,
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id,
    week_no, day, trans_time
  )


# Question 1 --------------------------------------------------------------
# Making the discounts positive values.
transaction_data <- transaction_data %>%
  mutate(retail_disc = abs(retail_disc)) %>%
  mutate(coupon_disc = abs(coupon_disc)) %>%
  mutate(coupon_match_disc = abs(coupon_match_disc))


# Question 2 --------------------------------------------------------------
# Creating three new variables following provided pricing logic.

transaction_data <- transaction_data %>%
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc)
         / quantity) %>%
  mutate(loyalty_price = (sales_value + coupon_match_disc) / quantity) %>%
  mutate(coupon_price  = (sales_value - coupon_disc) / quantity)


# Question 3 --------------------------------------------------------------
# Finding the number of distinct products which have a regular, loyalty, and
# coupon price which are less than $1.
regprice <- transaction_data %>%
  filter(regular_price <= 1) %>%
  select(product_id)
n_distinct(regprice)
#12,442 at the regular price

loyalprice <- transaction_data %>%
  filter(loyalty_price <= 1) %>%
  select(product_id)
n_distinct(loyalprice)
#20,113 at the loyalty price

couponprice <- transaction_data %>%
  filter(coupon_price <= 1) %>%
  select(product_id)
n_distinct(couponprice)
#22,273 at the coupon price


# Question 4 --------------------------------------------------------------
#Finding the proportion of baskets which have sales values of greater than $10.
basket10numer <- transaction_data %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  filter(basket_value > 10)
  
basket10denom <- transaction_data %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup()

nrow(basket10numer)/nrow(basket10denom)
# 0.6535243

# Question 5 ------------------------------------------------------------
# Finding the store which discounts the highest percent of good, and has a total
# amount of sales greater than $10,000.
transaction_data %>%
  filter(is.finite(regular_price), is.finite(loyalty_price), 
         regular_price > 0) %>%
  select(loyalty_price, regular_price, store_id, sales_value) %>%
  group_by(store_id) %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  summarise(sales_value_amount = sum(sales_value), 
            avgdisc = mean(pct_loyalty_disc)) %>%
  filter(sales_value_amount > 10000) %>%
  arrange(desc(avgdisc))

# Store 341, at 18.8%