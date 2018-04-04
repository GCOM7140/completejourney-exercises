library(tidyverse)
library(completejourney)
library(ggplot2)


transaction_data <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time
  )

#Question 1: Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
transaction_data<-
  transaction_data %>% 
  mutate(
  retail_disc = abs(retail_disc),
  coupon_disc = abs(coupon_disc),
  coupon_match_disc = abs(coupon_match_disc)
)

#Question 2: Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:
transaction_data <-
transaction_data %>% 
  mutate(
    regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
    loyalty_price = (sales_value + coupon_match_disc) / quantity,
    coupon_price  = (sales_value - coupon_disc) / quantity
)  

#Question 3: transaction_data includes 92,339 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty and coupon prices?
n_distinct(
  transaction_data %>% 
  filter(regular_price == 1 | regular_price < 1) %>% 
  select(product_id))
#12442

n_distinct(
  transaction_data %>% 
    filter(loyalty_price == 1 | loyalty_price < 1) %>% 
    select(product_id))
#20113

n_distinct(
  transaction_data %>% 
    filter(coupon_price == 1 | coupon_price < 1) %>% 
    select(product_id))
#22273

#Question 4: What proportion of baskets are over $10 in sales value?

transaction_data %>% 
  group_by(basket_id) %>% 
  mutate(basket_value = sum(sales_value)) %>% 
  summarise(proportion = mean(basket_value > 10))
#0.654

#Question 5: Which store with over $10K in total sales_value discounts its products the most for loyal customers?
storeid <-
transaction_data %>% 
  mutate(
  pct_loyalty_disc = 1 - (loyalty_price / regular_price)
  ) %>% 
  select(store_id, product_id, pct_loyalty_disc)
  group_by(store_id, pct_loyalty_disc) %>% 
  summarise(store_sales = sum(sales_value)) %>% 
  filter(store_sales > 10000) 


transaction_data %>% 
  filter(store_id %in% c(storeid))
  


transaction_data %>% 
  ggolot(aes())
