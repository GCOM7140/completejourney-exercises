####### Rosemary O'Hagan ###########
####### Customer Analytics HW #1 ###
####### 3/27/18 ####################


######################################################
##### Customer Journey Questions #####################
######################################################

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

##### Question 1 ###############################################################
# Change the discount variables (i.e., retail_disc, coupon_disc, 
# coupon_match_disc) from negative to positive.

transaction_data_q1 <- transaction_data %>% 
  mutate(retail_disc = abs(retail_disc),
         coupon_disc = abs(coupon_disc),
         coupon_match_disc = abs(coupon_match_disc)) 
transaction_data_q1

##### Question 2 ####################################################
# Create three new variables named regular_price, loyalty_price, and 
# coupon_price according to the following logic:
##### regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity
##### loyalty_price = (sales_value + coupon_match_disc) / quantity
##### coupon_price  = (sales_value - coupon_disc) / quantity

transaction_data_q2 <- transaction_data_q1 %>% 
  mutate(regular_price = 
           (sales_value + retail_disc + coupon_match_disc) / quantity,
         loyalty_price = 
           (sales_value + coupon_match_disc) / quantity,
         coupon_price  = 
           (sales_value - coupon_disc) / quantity)
transaction_data_q2

##### Question 3 ###############################################################
# transaction_data includes 92,339 unique product IDs. How many of these 
# products (not transactions!) had a regular price of one dollar or less? What 
# does this count equal for loyalty and coupon prices?

transaction_data_q3a <- filter(transaction_data, regular_price <= 1)  
n_distinct(select(transaction_dataQ3a,product_id)) #12,442

transaction_data_q3b <- filter(transaction_data, loyalty_price <= 1)  
n_distinct(select(transaction_dataQ3b,product_id)) #20,113

transaction_data_q3c <- filter(transaction_data, coupon_price <= 1)  
n_distinct(select(transaction_dataQ3c,product_id)) #22,273

##### Question 4 ####################################################
# What proportion of baskets are over $10 in sales value?

transaction_data %>% 
  group_by(basket_id) %>% 
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>% 
  summarize(proportion_over_10 = mean(basket_value > 10))
#### Approx. 65%
  
##### Question 5 #####################################################
# Which stores with over $10K in total sales_value discounts its products the most for loyal customers? 

transaction_data %>%
  filter(
    is.finite(regular_price), 
    is.finite(loyalty_price), 
    regular_price > 0
  ) %>%
  mutate(
    pct_loyalty_disc     = 1 - (loyalty_price / regular_price)
  ) %>%
  group_by(store_id) %>%
  summarize(
    total_sales_value    = sum(sales_value), 
    avg_pct_loyalty_disc = mean(pct_loyalty_disc)
  ) %>%
  filter(total_sales_value > 10000) %>%
  arrange(desc(avg_pct_loyalty_disc))