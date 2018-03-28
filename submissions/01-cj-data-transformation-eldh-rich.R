#----------------------------------------------------------------
#Rich Eldh
#Professor Boichek
#Customer Analytics
#Assignment #1
#Due Date: 27 March 2018
#----------------------------------------------------------------

#----------------------------------------------------------------
#Customer Journey Exercise
#----------------------------------------------------------------

library(tidyverse)
library(completejourney)

dat.transaction <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time
  )

#----------------------------------------------------------------
#Question 1
#----------------------------------------------------------------

dat.transaction <- mutate(dat.transaction,
    retail_disc       = abs(retail_disc),
    coupon_disc       = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )

dat.transaction

#----------------------------------------------------------------
#Question 2
#----------------------------------------------------------------

dat.transaction <- mutate(dat.transaction,
     regular_price     = (sales_value + retail_disc + coupon_match_disc) / quantity,
     loyalty_price     = (sales_value + coupon_match_disc) / quantity,
     coupon_price      = (sales_value - coupon_disc) / quantity
    )
dat.transaction <- select(dat.transaction,
      regular_price, loyalty_price, coupon_price, everything()
      )

dat.transaction


#----------------------------------------------------------------
#Question 3
#----------------------------------------------------------------

dat.transaction %>% 
  filter(regular_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
#12,442

dat.transaction %>% 
  filter(loyalty_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
#20,113

dat.transaction %>% 
  filter(coupon_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
#22,273

#----------------------------------------------------------------
#Question 4
#----------------------------------------------------------------

dat.transaction %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion_over_10 = mean(basket_value > 10))
#65.40%

#----------------------------------------------------------------
#Question 5
#----------------------------------------------------------------

dat.transaction %>%
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
