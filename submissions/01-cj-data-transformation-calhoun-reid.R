#Reid M. Calhoun
#March 27, 2018


########################################################
# Data Transformation Exercises - The Complete Journey #
########################################################
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

###############
# Question 1: #
###############
#Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
transaction_data <- transaction_data %>% 
  mutate(retail_disc = abs(retail_disc),
         coupon_disc = abs(coupon_disc),
         coupon_match_disc = abs(coupon_match_disc)
  )


###############
# Question 2: #
###############

#Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:
transaction_data <-  transaction_data %>% 
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
         loyalty_price = (sales_value + coupon_match_disc) / quantity, 
         coupon_price  = (sales_value - coupon_disc) / quantity
  )


###############
# Question 3: #
###############

#transaction_data includes 92,339 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty and coupon prices?

n_distinct(filter(transaction_data, regular_price <= 1) %>% 
             select(product_id)) #12442 regular priced 1 dollar or less

n_distinct(filter(transaction_data, loyalty_price <= 1) %>% 
             select(product_id)) #20113 loyalty price 1 dollar or less

n_distinct(filter(transaction_data, coupon_price <= 1) %>% 
             select(product_id)) #22273 coupon price 1 dollar or less


###############
# Question 4: #
###############

# What proportion of baskets are over $10 in sales value?

group_by(transaction_data, basket_id) %>% 
  summarize(basket_value = sum(sales_value, na.rm = TRUE)) %>% 
  ungroup(basket_id) %>% 
  select(basket_value) %>% 
  summarize(mean(basket_value > 10, na.rm = TRUE))
#65.4% of baskets are over $10 in sales value

###############
# Question 5: #
###############

#Which store with over $10K in total sales_value discounts its products the most for loyal customers?

group_by(transaction_data,store_id) %>% 
  filter(sum(sales_value) > 10000) %>% 
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>% 
  summarize(loyalty_disc = mean(pct_loyalty_disc, na.rm=TRUE)) %>%
  arrange(desc(loyalty_disc))
#Store 341 discounts its products the most for loyal customers


