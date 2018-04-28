#Author: Michael Starnes
#Assignment: CJ Data Transformation
#Course: Customer Analytics


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


#Question 1: Reversing Variable Values

transactiondata <- transaction_data %>% 
  mutate(
    coupon_disc = abs(coupon_disc)
    retail_disc= abs(retail_disc)
    coupon_match_disc = abs(coupon_match_disc)
    )

#Question 2: Creating new variables
(transaction_data <- transaction_data %>% 
    mutate(
      regular_price     = (sales_value + retail_disc + coupon_match_disc) / 
        quantity,
      loyalty_price     = (sales_value + coupon_match_disc) / 
        quantity,
      coupon_price      = (sales_value - coupon_disc) / 
        quantity
    ) %>% 
    select(loyalty_price,regular_price,  coupon_price, everything())
)


#Question 3: Filtering Products
  transaction_data %>% 
      filter(regular_price <= 1.00) %>% 
      select(product_id) %>% 
      n_distinct()
  #There are 12442 items that have a price of 99 cents or less and are unique
  
  transaction_data %>% 
  filter(loyalty_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
  #there are 20113 items with a loyalty price of less than one dollar that are distinct.
  
  transaction_data %>%
    filter(coupon_price <= 1) %>% 
    select(product_id) %>% 
    n_distinct()
  #22273 items have coupon prices that are less than a dollar
  
#Question 4: Proportions of baskets
  transaction_data %>% 
      group_by(basket_id) %>% 
      summarize(basket_value= sum(sales_value)) %>% 
      ungroup() %>% 
      summarize(greaterthan10share = mean(basket_value > 10.00))
    #Baskets costing over ten dollars comprised 65.4% of baskets
  
#Question 5: finding discount

  transaction_data %>% 
    filter(
        is.finite(regular_price),
        is.finite(loyalty_price),
        regular_price >0
    ) %>% 
    mutate(
      discountpercent = 1- (loyalty_price/ regular_price)
      %>% 
        group_by(store_id) %>% 
        summarize(
          salesvaltotal =sum(sales_value), 
          percentloyaltydiscount = mean  (pct_loyalty_disc)
        )
          %>%
            filter(total_sales_value > 10000) %>%
            arrange(desc(avg_pct_loyalty_disc))
    
      #about 19 percent was the highest discount off of full value for a store, and store 341 was the store with the greatest discount overall
