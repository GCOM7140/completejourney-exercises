library(tidyverse)
library(completejourney)

# Question1
transactions <- transactions %>% 
  mutate(
    retail_disc = abs(retail_disc),
    coupon_disc = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )

# Question2
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

# Question3
# regular price
transactions %>% 
  filter(regular_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()

# loyalty price
transactions %>% 
  filter(loyalty_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()

#coupon price
transactions %>% 
  filter(coupon_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()


# Question4
transactions %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion_over_10 = round(mean(basket_value > 10) * 100, 0))
# 66 percent


# Question5
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

# The store with id 341 discounts its products the most.