
#' --- 
#' title: "Customer Journey exercise 4" 
#' author: Robby Byrne 
#' date: April 17, 2018 
#' output: github_document 
#' ---
#' 

knitr::opts_chunk$set(echo = TRUE) 

library(tidyverse)
library(completejourney)

##### Question 1 ######

?coupon_redempt
?campaign_table

amount_redeemed <- length(unique(coupon_redempt$household_key))
amount_sent <- length(unique(campaign_table$household_key))
amount_redeemed / amount_sent
## 0.2739899 or 27.4% 

####### Question 2


amount_sent - amount_redeemed
# 1150 did not  get redeemed 

####### Question 3 ##########

left_join(coupon %>% count(coupon_upc),
          coupon_redempt %>% count(coupon_upc), by = 'coupon_upc') %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))
## 49% were redeemed 

####### Question 4 ########


left_join(transaction_data, product) %>%
  group_by(sub_commodity_desc, week_no) %>%
  summarize(sales_value_category = sum(sales_value, na.rm = TRUE)) %>%
  filter(week_no == 52 | week_no == 102) %>%
  mutate(revenue_growth = ifelse(lag(sales_value_category) >= 100,
                                 (sales_value_category - 
                                    lag(sales_value_category)) /
                                   lag(sales_value_category), NA)) %>%
  arrange(desc(revenue_growth))
# select beef grew the most at 85.2 % 







