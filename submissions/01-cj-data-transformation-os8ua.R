## Oliver Song (os8ua)

library(tidyverse)
library(completejourney)

## Question 1 - Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive

transactions %>%
  mutate(retail_disc = abs(retail_disc),
         coupon_disc = abs(coupon_disc),
         coupon_match_disc = abs(coupon_match_disc))

## Question 2 - Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:

transactions %>%
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
         loyalty_price = (sales_value + coupon_match_disc) / quantity,
         coupon_price  = (sales_value - coupon_disc) / quantity)

## Question 3 - The transactions dataset includes 68,509 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty price and coupon price?

transactions %>%
  filter(regular_price <= 1) %>%
  