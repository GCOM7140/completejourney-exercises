#' ---
#' title: "Answers to the CJ Data Wrangling Exercise"
#' author: Noah Rodammer
#' date: April 17, 2018
#' output: github_document
#' ---

library(tidyverse)
library(completejourney)

#' organize columns systematically
left_join(transaction_data, product) %>% 
  left_join(hh_demographic) %>% 
  filter(
    quantity != 0
  ) %>% 
  mutate(
    regular_price  = (sales_value + retail_disc + coupon_match_disc) /
      quantity,
    loyalty_price  = (sales_value + coupon_match_disc) / 
      quantity,
    coupon_price   = (sales_value - coupon_disc) / 
      quantity,
    purchase_price = ifelse(coupon_disc > 0, coupon_price, 
                            ifelse(retail_disc > 0, loyalty_price,
                                   regular_price))
  ) -> my_transaction_data


#' **Question 1**: What percent of households that received the retailer's 
#' weekly mailer redeemd at least one coupon?
coupon
coupon_redempt
campaign_table

countRedeem<-inner_join(  campaign_table %>% count(household_key),
             coupon_redempt %>% count(household_key), by = 'household_key')  

totalCount<-  campaign_table %>% count(household_key)
percent<-nrow(countRedeem)/nrow(totalCount)
percent

#' 27.4% 
 
#' **Question 2**: How many households did not redeem a coupon?

nrow(totalCount)-nrow(countRedeem)

#' 1,150 houses

#' **Question 3**: What percent of coupons promoted in the retailer's weekly 
#' mailer got redeemed at least once?

countCouponRedeem<-inner_join(  coupon %>% count(coupon_upc),
                          coupon_redempt %>% count(coupon_upc), 
                          by = 'coupon_upc')  

totalCountCoupon<-  coupon %>% count(coupon_upc)
percentUse<-nrow(countCouponRedeem)/nrow(totalCountCoupon)
percentUse

#' 49%

#' **Question 4**: Using the transaction_data and product datasets, determine 
#' which product category (i.e., sub_commodity_desc) grew the most in terms of
#' revenue for the retailer in the second half of the study period (i.e.,
#' between week_no == 52 and week_no == 102). Only consider product categories 
#' that had over $100 in revenue in week 52, and calculate revenue growth as a 
#' percentage of category revenue in week 52.

#'Here are some suggested steps:
  
#' 1) Join the transaction_data and product datasets.
#' 2)Group the data by sub_commodity_desc and week_no.
#' 3)Calculate sales_value at the category level.
#' 4)Filter the data to only include category revenues in weeks 52 and 102.
#' 5)Create a new variable called revenue_growth, making use of the lag() 
#' function. Recognize that you only want to consider categories that had 
#' category revenues of at least $100 in week 52.
#' 6) Arrange the data in descending order according to revenue_growth.
?lag
left_join(transaction_data,product) %>%
  group_by(sub_commodity_desc,week_no) %>%
  summarize(sales_value_commodity = sum(sales_value,na.rm=TRUE)) %>%
  filter((week_no==52)|(week_no==102)) %>%
  mutate(
    revenue_growth = ifelse(lag(sales_value_commodity)>=100,
                            (sales_value_commodity-lag(sales_value_commodity))
                            /lag(sales_value_commodity),NA)
    ) %>%
  arrange(-revenue_growth)

#' Select Beef grew the most in the second half of the period by 85.2%  
