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

#Question 1: Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
transaction_data <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time
  ) %>%
  mutate(abs_retail_disc = abs(retail_disc), abs_coupon_disc = abs(coupon_disc), abs_coupon_match_disc = abs(coupon_match_disc)) %>%
  
  #Question 2: Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:
  mutate (regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
          loyalty_price = (sales_value + coupon_match_disc) / quantity,
          coupon_price  = (sales_value - coupon_disc) / quantity)

#Question 3: transaction_data includes 92,339 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty and coupon prices?
str(transaction_data)
transaction_data %>%
  filter(regular_price <= 1) %>%
  n_distinct(transaction_data$product_id)    #954883

#Question 4: What proportion of baskets are over $10 in sales value?
transaction_data %>%
  total_basket <- n_distinct(transaction_data$basket_id)
basket_greater_than_10 <- filter(transaction_data, sales_value >= 10)
porp <- n_distinct(basket_greater_than_10) / total_basket #0.307

#Question 5: Which store with over $10K in total sales_value discounts its products the most for loyal customers?
transaction_data %>%
  group_by(store_id) %>%
  filter(sum(sales_value) > 10000) %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  arrange(desc(pct_loyalty_disc)) %>%
  select(pct_loyalty_disc)
