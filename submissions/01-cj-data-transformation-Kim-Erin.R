library(tidyverse)
library(completejourney)
library(dplyr)
library(nycflights13)


# CustomerJourney 1 -----------------------------------------------------------------------

transaction_data %>% 
  select(basket_id, sales_value) %>% 
  arrange(basket_id, desc(sales_value))


# CustomerJourney 2 -----------------------------------------------------------------------

transaction_data %>% 
  mutate(regular_price = (sales_value - (retail_disc + coupon_match_disc)) / quantity,
         loyalty_price = regular_price + (retail_disc / quantity)) %>% 
  select(regular_price, loyalty_price, everything())


# CustomerJourney 3 -------------------------------------------------------

transaction_data %>% 
  mutate(regular_price = (sales_value - (retail_disc + coupon_match_disc)) / quantity,
         loyalty_price = regular_price + (retail_disc / quantity)) %>%
  filter(regular_price <= 1) %>% 
  mutate(count = n_distinct(product_id)) %>% 
  select(count, regular_price, product_id, everything())

transaction_data %>% 
  mutate(regular_price = (sales_value - (retail_disc + coupon_match_disc)) / quantity,
         loyalty_price = regular_price + (retail_disc / quantity)) %>%
  filter(loyalty_price <= 1) %>% 
  mutate(count_loyalty = n_distinct(product_id)) %>% 
  select(count_loyalty, loyalty_price, product_id, everything())


# CustomerJourney4 --------------------------------------------------------

transaction_data %>% 
  group_by(basket_id) %>%
  mutate(basket_value = sum(sales_value)) %>%
  ungroup() %>% 
  summarize(bask = mean(basket_value > 10)) %>% 
  ungroup()

# CustomerJourney5 --------------------------------------------------------

transaction_data %>% 
  group_by(store_id) %>% 
  mutate(total_sales_value = sum(sales_value),
    regular_price = (sales_value - (retail_disc + coupon_match_disc)) / quantity,
    loyalty_price = regular_price + (retail_disc / quantity),
    pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>% 
  ungroup() %>% 
  filter(total_sales_value > 10000) %>% 
  arrange(desc(pct_loyalty_disc)) %>% 
  select(store_id, pct_loyalty_disc, loyalty_price, regular_price, total_sales_value, everything())