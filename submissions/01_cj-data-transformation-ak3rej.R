#install.packages("remotes")
#remotes::install_github("bradleyboehmke/completejourney")

library(tidyverse)
library(completejourney)
get_data(which = "transactions", verbose = FALSE)


#Question 1: Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.

transactions %>% 
  mutate(
    retail_disc       = abs(retail_disc),
    coupon_disc       = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )  %>% 
  select(household_id:product_id, retail_disc:coupon_match_disc, everything())
  

#Question 2: Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:
#regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity
#loyalty_price = (sales_value + coupon_match_disc) / quantity
#coupon_price  = (sales_value - coupon_disc) / quantity

transactions %>% 
  mutate(
    regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
    loyalty_price = (sales_value + coupon_match_disc) / quantity,
    coupon_price  = (sales_value - coupon_disc) / quantity
  )  %>% 
  select(household_id:product_id, regular_price:coupon_price, everything())


#Question 3: The transactions dataset includes 68,509 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty price and coupon price?

transactions %>%
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / (quantity)) %>%
  filter(regular_price <= 1) %>%
  select(product_id)%>%
  n_distinct()

transactions %>%
  mutate(loyalty_price = (sales_value + coupon_match_disc) / quantity) %>%
  filter(loyalty_price <= 1) %>%
  select(product_id)%>%
  n_distinct()

transactions %>%
  mutate(coupon_price  = (sales_value - coupon_disc) / quantity) %>%
  filter(coupon_price <= 1) %>%
  select(product_id)%>%
  n_distinct()

#Question 4: What proportion of baskets are over $10 in sales value?
#A proportion is simply another name for a mean of a set of zeroes and ones. The mean of the 5 values, 1   0   0   1   0, is the number of ones divided by 5, or 2/5 or 0.4. So 40% Values are true and given proportion is 0.4

transactions %>%
  group_by(basket_id) %>%
  summarize(total_basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion = mean(total_basket_value > 10) * 100)


#Question 5: Which store with over $10K in total sales_value discounts its products the most for loyal customers?

transactions %>%
  filter(
    is.finite((sales_value + retail_disc + coupon_match_disc) / quantity), 
    is.finite((sales_value + coupon_match_disc) / quantity), 
    ) %>%
  mutate(
    regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
    loyalty_price = (sales_value + coupon_match_disc) / quantity,
    discount_percentage = (((regular_price - loyalty_price) / regular_price) * 100) 
  ) %>%
  group_by(store_id) %>%
  summarize(
    total_sales_value = sum(sales_value), 
    avg_discount_percentage = mean(discount_percentage)
  ) %>%
  filter(total_sales_value > 10000) %>%
  arrange(desc(avg_discount_percentage))
  
