library(tidyverse)
library(completejourney)


# Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.

transactions <- transactions %>%
  mutate(
    retail_disc       = (retail_disc) ,
    coupon_disc       = abs(coupon_disc) ,
    coupon_match_disc = abs(coupon_match_disc)
  )
view(transactions)

# Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:

(transactions <- transactions %>% 
    mutate(
      regular_price = (sales_value + retail_disc + coupon_match_disc) / 
        quantity,
      loyalty_price = (sales_value + coupon_match_disc) / 
        quantity,
      coupon_price  = (sales_value - coupon_disc) / 
        quantity
    )
)
view(transactions)

# The transactions dataset includes 68,509 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty price and coupon price?

transactions %>%
  filter(regular_price <= 1) %>%
  select(product_id) %>%
  n_distinct()

transactions %>%
  filter(loyalty_price <= 1) %>%
  select(product_id) %>%
  n_distinct()

transactions %>%
  filter(coupon_price <= 1) %>%
  select(product_id) %>%
  n_distinct()

# What proportion of baskets are over $10 in sales value?

transactions %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  summarize(proportion_over10 = round(mean(basket_value > 10) * 100, 0 ))

# Which store with over $10K in total sales_value discounts its products the most for loyal customers?

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
