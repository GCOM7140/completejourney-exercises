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

#question 1:

transaction_data <- transaction_data %>% 
  mutate(
    retail_disc       = abs(retail_disc),
    coupon_disc       = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )

#question 2:

transaction_data <- transaction_data %>% 
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / 
           quantity, 
         loyalty_price = (sales_value + coupon_match_disc) / 
           quantity,
         coupon_price = (sales_value - coupon_disc) / quantity)
transaction_data_2 <- select(transaction_data, regular_price, loyalty_price, coupon_price)
head(transaction_data_2)

#question 3

transaction_data %>%  filter(regular_price <= 1) %>% select(product_id) %>% 
  n_distinct()

#12442 products above a dollar

transaction_data %>% filter(loyalty_price <= 1) %>% select(product_id) %>% 
  n_distinct()

#20113 for loyalty 

transaction_data %>% filter(coupon_price <= 1) %>% select(product_id) %>% 
  n_distinct()

#22273

#question 4
transaction_data %>%group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>% summarize(proportion_over_10 = mean(basket_value > 10))

?ungroup

#0.654

#question 5

?is.finite
transaction_data %>%
  filter(is.finite(regular_price), is.finite(loyalty_price),regular_price > 0 )%>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  group_by(store_id) %>%
  summarize( total_sales_value = sum(sales_value),avg_pct_loyalty_disc = mean(pct_loyalty_disc)) %>%
  filter(total_sales_value > 10000) %>% arrange(desc(avg_pct_loyalty_disc))

#store 341 