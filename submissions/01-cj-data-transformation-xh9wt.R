library(tidyverse)
library(completejourney)

# Question 1
# Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
transactions <- transactions %>%
  mutate(
    retail_disc       = abs(retail_disc),
    coupon_disc       = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )

# Question 2
# Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:
transactions  <- transactions %>%
  mutate(
    regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
    loyalty_price = (sales_value + coupon_match_disc) / quantity,
    coupon_price  = (sales_value - coupon_disc) / quantity
  )

# Question 3
# The transactions dataset includes 68,509 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty price and coupon price?
transactions %>%
  filter(regular_price <= 1) %>%
  select(product_id) %>%
  n_distinct()

# 8698 unique regular price products are one dollar or less
transactions %>%
  filter(loyalty_price <= 1) %>%
  select(product_id) %>%
  n_distinct()

# 14043 unique loyalty priced products are one dollar or less
transactions %>%
  filter(coupon_price <= 1) %>%
  select(product_id) %>%
  n_distinct()

# 15676 unique coupon priced products are one dollar or less
# This indicates that loyalty and coupon pricing discounts a significant number of products to one dollar or less

# Question 4
# What proportion of baskets are over $10 in sales value?
transactions %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion_over_10 = round(mean(basket_value > 10) * 100, 0))
# approximately 66% of customers have baskets larger than 10 dollars

# Question 5
# Which store with over $10K in total sales_value discounts its products the most for loyal customers?
transactions %>%
  filter(
    is.finite(regular_price),
    is.finite(loyalty_price),
    regular_price > 0
  ) %>%
  mutate(
    pct_loyalty_disc = 1 - (loyalty_price / regular_price)
  ) %>%
  group_by(store_id) %>%
  summarise(
    total_sales_value = sum(sales_value),
    avg_pct_loyalty_disc = mean(pct_loyalty_disc)
  ) %>%
  filter(total_sales_value > 10000) %>%
  arrange(desc(avg_pct_loyalty_disc))
# store 341 gives the largest average loyalty discount at 18.7%  