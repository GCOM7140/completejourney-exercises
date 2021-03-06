install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("bradleyboehmke/completejourney")

library(tidyverse)
library(completejourney)
library(dplyr)
---
  
  ## Question 1:## Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
  
  # Notes:
  
  # mutate() adds new variables and preserves existing one.(Adding new column to the exsiting data frame)/ Calculating returns to the same variables if not assign to a new variable
  
  # %>%: pipe operator means calculate from left to right as a chain function. For example: x %>% f %>% g %>% h can be rewritten as h(g(f(x)))
  
  transactions <- transactions %>% 
  mutate(
    retail_disc       = abs(retail_disc),
    coupon_disc       = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )

transactions

---
  
  ## Question 2: ## Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:
  
  transactions <- transactions %>% 
  mutate (regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
          loyalty_price = (sales_value + coupon_match_disc) / quantity,
          coupon_price  = (sales_value - coupon_disc) / quantity )
transactions
---
  
  ## Question 3: ## The transactions dataset includes 68,509 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty price and coupon price?
  
  # In mathematics: n_distinct(select(filter(transactions, regular_price =< 1), product_id ))
  
  transactions %>% filter(regular_price <= 1) %>% select(product_id) %>% n_distinct()

transactions %>% filter(loyalty_price <= 1) %>% select(product_id) %>% n_distinct()

transactions %>% filter(coupon_price <= 1) %>% select(product_id) %>% n_distinct()

---
## Question 4 ##: What proportion of baskets are over $10 in sales value?


## Hint: ## You need to use [`group_by()`][summarize], [`summarize()`][summarize],
##and [`ungroup()`][ungroup]. As a last step, you can calculate the proportion by
##taking the mean of `TRUE/FALSE` values, using `mean(basket_value > 10)` to get
##the proportion over $10.


# In mathematics: summarize(ungroup(summarize(group by())))

transactions %>% group_by(basket_id) %>% summarize(sales_value > 10) %>% 
  ---
  
## Question 5 ##: Which store with over $10K in total `sales_value` discounts its products the most for loyal customers? 
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
  
  