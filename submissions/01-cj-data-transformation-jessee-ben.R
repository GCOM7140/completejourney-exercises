#Assignment 1
#Ben Jessee
#Customer Journey Problems

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

#1
transaction_data <- transaction_data %>% 
  mutate(
    retail_disc       = abs(retail_disc),
    coupon_disc       = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )

#2
transaction_data <- transaction_data %>% 
  mutate(
    regular_price = (sales_value + retail_disc + coupon_match_disc) / 
      quantity,
    loyalty_price = (sales_value + coupon_match_disc) /
      quantity,
    coupon_price  = (sales_value - coupon_disc) / 
      quantity
  ) %>% 
  select(regular_price, loyalty_price, coupon_price, everything())
)

#3

transaction_data %>% 
    filter(regular_price <=1) %>% 
    select(product_id) %>% 
    n_distinct()

  # =12442

transaction_data %>% 
    filter(loyalty_price <=1) %>% 
    select(product_id) %>% 
    n_distinct ()


  # =20113

transaction_data %>% 
    filter(coupon_price <=1) %>% 
    select(product_id) %>% 
    n_distinct ()

  # =22273

#4
transaction_data %>%
    group_by(basket_id) %>% 
    summarize(basket_val = sum(sales_value)) %>% 
    ungroup() %>% 
    summarize(prop_10 = mean(basket_val > 10))

  # =65.4%

#5
transaction_data %>% 
    filter(
      is.finite(regular_price),
      is.finite(loyalty_price),
      regular_price > 0
    ) %>% 
    mutate(
      pct_loyalty_disc = 1 - (loyalty_price / regular_price)
    ) %>% 
    group_by(store_id) %>%
    summarize(
      total_sales_value = sum(sales_value),
      avg_pct_loyalty_disc = mean(pct_loyalty_disc)
    ) %>% 
    filter(total_sales_value > 10000) %>% 
    arrange(desc(avg_pct_loyalty_disc))


    #Store 341 (18.8%)
  