## Chapter 5 R4DS Exercises -- Data Transformation Exercises ##

library(tidyverse)
library(completejourney)

transaction_data <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time)

transaction_data


#### ----- Question 1 : Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.

transaction_data <- transaction_data %>%
  mutate(
    retail_disc = abs(retail_disc),
    coupon_disc = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )

#### ----- Question 2: Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:

transaction_data <- transaction_data %>%
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity) %>% 
  mutate(loyalty_price = (sales_value + coupon_match_disc) / quantity) %>% 
  mutate(coupon_price  = (sales_value - coupon_disc) / quantity)

# Question 3: transaction_data includes 92,339 unique product IDs. How many of
# these products (not transactions!) had a regular price of one dollar or less?
# What does this count equal for loyalty and coupon prices?

transaction_data %>%
  filter(regular_price <= 1.00) %>%
  select(product_id)  %>%
  n_distinct()

# Answer: there are 12,442 products that are $1 or less

transaction_data %>%
  select(loyalty_price, product_id)  %>%
  filter(loyalty_price < 1.00) %>%
  summarise(count_all = n_distinct(product_id))

# Answer: 

transaction_data %>%
  select(coupon_price, product_id)  %>%
  filter(coupon_price < 1.00) %>%
  summarise(count_all = n_distinct(product_id))

# Answer:

#### ----- Question 4: What proportion of baskets are over $10 in sales value?

transaction_data %>%
  select(sales_value, basket_id, product_id) %>%
  group_by(sales_value, basket_id, product_id) %>%
  summarise(transaction_data, basket_id, mean(basket_value > 10))

# Answer: 

## ----- Question 5: Which store with over $10K in total sales_value discounts its products the most for loyal customers?

transaction_data %>%
  select(sales_value, loyalty_price, coupon_disc, regular_price) %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  filter(sales_value, pct_loyalty_disc) %>%
  
  
  
  
  
  