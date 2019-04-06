devtools::install_github("bradleyboehmke/completejourney")

library(tidyverse)
library(completejourney)

#Question 1: Change the discount variables (i.e., retail_disc, coupon_disc,
#coupon_match_disc) from negative to positive.

transactions <- transactions %>%  mutate(retail_disc = abs(retail_disc), coupon_disc= abs(coupon_disc),coupon_match_disc = abs(coupon_match_disc))

#Question 2: Create three new variables named regular_price, loyalty_price, and
#coupon_price according to the following logic:

regular_price <- (transactions$sales_value + transactions$retail_disc + transactions$coupon_match_disc) / transactions$quantity
loyalty_price <- (transactions$sales_value + transactions$coupon_match_disc) / transactions$quantity
coupon_price  <- (transactions$sales_value - transactions$coupon_disc) / transactions$quantity

(transactions <- transactions %>% 
    mutate(regular_price,loyalty_price,coupon_price))

#Question 3: The transactions dataset includes 68,509 unique product IDs. How
#many of these products (not transactions!) had a regular price of one dollar or
#less? What does this count equal for loyalty price and coupon price?
#Hint: After filtering, select the product_id column, then count the number of unique products using the n_distinct() function.


transactions %>% 
  filter(regular_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()

#Question 4: What proportion of baskets are over $10 in sales value? Hint: You
#need to use group_by(), summarize(), and ungroup(). As a last step, you can
#calculate the proportion by taking the mean of TRUE/FALSE values, using
#mean(basket_value > 10) to get the proportion over $10.

transactions %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion_over_10 = round(mean(basket_value > 10) * 100, 0))

#Question 5: Which store with over $10K in total sales_value discounts its products the most for loyal customers?
#Hint: You can calculate loyalty discount as a percentage of regular price using the following logic:

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

