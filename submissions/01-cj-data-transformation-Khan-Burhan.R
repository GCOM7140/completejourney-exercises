####Burhan Khan homework1 complete journey####


####Customer Journey exercises####
library(tidyverse)
library(completejourney)
transaction_data <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time)

#Question 1: Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
str(transaction_data)
transaction_data
transaction_data <- mutate(transaction_data, retail_disc = abs(retail_disc), 
                           coupon_disc = abs(coupon_disc), 
                           coupon_match_disc = abs(coupon_match_disc))

#Question 2:  Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:
transaction_data <- mutate(transaction_data, regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
                           loyalty_price = (sales_value + coupon_match_disc) / quantity,
                           coupon_price  = (sales_value - coupon_disc) / quantity)

#Question 3: transaction_data includes 92,339 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty and coupon prices?
str(transaction_data)
reg_under_1 <- filter(transaction_data, regular_price <= 1)
n_distinct(reg_under_1$product_id)

coupon_under_1 <- filter(transaction_data, coupon_price <= 1)
n_distinct(coupon_under_1$product_id)

loyalty_under_1 <- filter(transaction_data, loyalty_price <= 1)
n_distinct(loyalty_under_1$product_id)
#Regular under $1 = 12442, Coupon under $1 = 22273, Loyalty under $1 = 20113

#Question 4: What proportion of baskets are over $10 in sales value?
transaction_baskets <- transaction_data %>% group_by(basket_id) %>% 
  summarize(total = sum(sales_value))

transaction_baskets$total <- ifelse(transaction_baskets$total > 10, TRUE, FALSE)
mean(transaction_baskets$total)
#65.3% are over $10 in sales value


#Question 5: Which store with over $10K in total sales_value discounts its products the most for loyal customers?
stores_over_10000 <- transaction_data %>%
  group_by(store_id) %>% summarize(total_sales = sum(sales_value)) %>% filter(total_sales > 10000)

transaction_data %>% filter(store_id %in% stores_over_10000$store_id) %>% mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>% group_by(store_id) %>% arrange(desc(pct_loyalty_disc))
#446