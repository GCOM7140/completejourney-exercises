# install.packages("devtools")
devtools::install_github("bradleyboehmke/completejourney")

library(tidyverse)
library(completejourney)

options(warnPartialMatchArgs = FALSE)

#1
transactions <- transactions
transactions <-mutate(transactions, retail_disc = abs(retail_disc), coupon_disc= abs(coupon_disc), coupon_match_disc = abs(coupon_match_disc))

#2
transactions <-mutate(transactions, regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity, loyalty_price = (sales_value + coupon_match_disc) / quantity,coupon_price  = (sales_value - coupon_disc) / quantity)

#3
#regular price
n_distinct(select(filter(transactions, regular_price <= 1), product_id))

#loyalty price
n_distinct(select(filter(transactions, loyalty_price <= 1), product_id))

#coupon price
n_distinct(select(filter(transactions, coupon_price <= 1), product_id))

#4
transactions %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion_over_10 = round(mean(basket_value > 10) * 100, 0))

#5
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
