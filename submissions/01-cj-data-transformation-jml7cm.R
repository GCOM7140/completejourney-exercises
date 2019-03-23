### Complete Journey Exercises - JML ###

# Make sure the R Studio you are using has these packages installed every time you begin work. 

library(tidyverse)
library(completejourney)
library(dplyr)

### Question 1 ###

# Change the discount variables (i.e., `retail_disc`, `coupon_disc`, `coupon_match_disc`) from negative to positive.

dat.transactions <- transactions

transactions <- transactions %>% 
  dplyr::mutate(
    retail_disc       = abs(retail_disc),
    coupon_disc       = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  ) 

# ***Prof. Boichuk for whatever reason I had an issue getting R to recognize the Transactions data from the Complete Journey folder. I will swing by office hours next week to see if I can figure out what the issue is.***

### Question 2 ###

# Create three new variables named `regular_price`, `loyalty_price`, and `coupon_price` according to the following logic:

transactions <- transactions %>%
  
regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity
loyalty_price = (sales_value + coupon_match_disc) / quantity
coupon_price  = (sales_value - coupon_disc) / quantity

### Question 3 ###

# The `transactions` dataset includes 68,509 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty price and coupon price?

transactions %>% 
  filter(regular_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()

# Next we need to check for loyalty price which is different. We will run a similar function with one minor change. 

transactions %>% 
  filter(loyalty_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()

### Question 4 ###

# What proportion of baskets are over $10 in sales value? 

# This question needs the Group By function which is an important function in R.

transactions %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion_over_10 = round(mean(basket_value > 10) * 100, 0))

### Question 5 - last question in this CJ exercise ###

# Which store with over $10K in total `sales_value` discounts its products the most for loyal customers? 

transactions %>%
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

# ***Prof. Boichuk, on my personal computer I had some issues with the regular_price object. I assume it was user error, but I would also like to talk to you about that issue and how to correctly find the code for #5, without having issues.***

### End of exercise ###
