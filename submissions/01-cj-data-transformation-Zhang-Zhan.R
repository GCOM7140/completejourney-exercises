library(tidyverse)
library(completejourney)

# Question1 Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
transactions <- transactions %>%
  mutate(
    retail_disc = abs(retail_disc),
    coupon_disc = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )


# Question2 Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic
transactions <- transactions %>%
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
        loyalty_price = (sales_value + coupon_match_disc) / quantity,
        coupon_price  = (sales_value - coupon_disc) / quantity)


# Question3 The transactions dataset includes 68,509 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty price and coupon price?
transactions%>%
  filter(regular_price <= 1) %>%
  select(product_id) %>%
  n_distinct() 
#----> 8698 products (not transactions!) had a regular price of one dollar or less
  
transactions%>%
  filter(loyalty_price <= 1) %>%
  select(product_id) %>%
  n_distinct() 
#----> 14043 products (not transactions!) had a loyalty price of one dollar or less

transactions%>%
  filter(coupon_price <= 1) %>%
  select(product_id) %>%
  n_distinct() 
#----> 15676 products (not transactions!) had a coupon price of one dollar or less


# Question4 What proportion of baskets are over $10 in sales value? ----> 66.1% of baskets are over $10 in sales value
transactions%>%
  group_by(basket_id) %>%
  summarise(salesvalue = sum(sales_value))%>%
  ungroup()%>%
  summarise(proporation=mean(salesvalue>10))


# Question5 Which store with over $10K in total sales_value discounts its products the most for loyal customers? ----> store 341 discounts its products the most for loyal customers with about 18.7% 

transactions %>%
  filter(
    is.finite(loyalty_price), 
    is.finite(regular_price), 
    regular_price > 0
  ) %>% 
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>% 
  group_by(store_id) %>% 
  summarise(total_sales_value= sum(sales_value),mean_pct_loyalty_disc = mean(pct_loyalty_disc)) %>% 
  filter(total_sales_value >10000) %>%
  arrange(desc(mean_pct_loyalty_disc))
