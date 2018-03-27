#############
# FILE INFO #
#############

# Title: GCOMM 7140 Homework 1
# Author: Noah Rodammer


#################
# LOAD PACKAGES #
#################

library(tidyverse)
library(dplyr)
library(devtools)
library(completejourney)


#organize columns systematically
transaction_data <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time
  )

######################
#  COMPLETE JOURNEY  #
#      EXERCISES     #
######################


#################
#  QUESTION #1  #
#################

#Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
transaction_data<-mutate(transaction_data,
                         retail_disc=abs(retail_disc),
                         coupon_disc=abs(coupon_disc),
                         coupon_match_disc=abs(coupon_match_disc))

#################
#  QUESTION #2  #
#################

#Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:

#regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity
#loyalty_price = (sales_value + coupon_match_disc) / quantity
#coupon_price  = (sales_value - coupon_disc) / quantity

transaction_data<-mutate(transaction_data,
                         regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity, 
                         loyalty_price = (sales_value + coupon_match_disc) / quantity, 
                         coupon_price  = (sales_value - coupon_disc) / quantity)

select(transaction_data,regular_price,loyalty_price,coupon_price,everything())

#################
#  QUESTION #3  #
#################

#transaction_data includes 92,339 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty and coupon prices?

transaction_data %>%
  filter(regular_price<=1) %>%
 select(product_id) %>%
  n_distinct()

##12,442


transaction_data %>%
  filter(loyalty_price<=1) %>%
  select(product_id) %>%
  n_distinct()
##20,113

transaction_data %>%
  filter(coupon_price<=1) %>%
  select(product_id) %>%
  n_distinct()
##22,273

#################
#  QUESTION #4  #
#################

#What proportion of baskets are over $10 in sales value?

transaction_data %>%
  group_by(basket_id) %>%
  summarize(cost=sum(sales_value)) %>%
  ungroup() %>%
  summarize(prop = mean(cost>10))

##65.4%


#################
#  QUESTION #5  #
#################

#Which store with over $10K in total sales_value discounts its products the most for loyal customers?

transaction_data %>%
  filter(regular_price>0,  
         is.finite(regular_price), 
         is.finite(loyalty_price) ) %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  group_by(store_id) %>%
  summarize(total=sum(sales_value),
            averageDisc=mean(pct_loyalty_disc)) %>%  
  filter(total>10000) %>%
  arrange(desc(averageDisc))

##store 341 at 18.8%
