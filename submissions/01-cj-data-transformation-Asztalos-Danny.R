#install.packages(c("tidyverse", "devtools"))

devtools::install_github("bradleyboehmke/completejourney")


library(tidyverse)
library(completejourney)



#Question 1: Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
transactions <- transactions %>% 
  mutate(
    retail_disc       = abs(retail_disc),
    coupon_disc       = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )

#Question 2: Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:

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

#Question 3: The transactions dataset includes 68,509 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty price and coupon price?

#Hint: After filtering, select the product_id column, then count the number of unique products using the n_distinct() function.

transactions %>% 
  filter(regular_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()

# 8,698 products had a regular price less than or equal to $1.00.

transactions %>% 
  filter(loyalty_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()

# This count for loyalty price is 14,043 products.

transactions %>% 
  filter(coupon_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()