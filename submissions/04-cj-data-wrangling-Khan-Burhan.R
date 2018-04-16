#' ---
#' title: "CJ Exercise #4"
#' author: Burhan Khan
#' date: April 14, 2018
#' output: github_document
#' ---

library(tidyverse)
library(completejourney)


#Question 1:
count_redeemed <- length(unique(coupon_redempt$household_key))
count_sent <- length(unique(campaign_table$household_key))
count_redeemed / count_sent
#27.4% 

#Question 2:
count_sent - count_redeemed
#1150

#Question 3:
coupon_product <- inner_join(coupon, product, by = "product_id")
coupon_product_redeem <- inner_join(coupon_product, coupon_redempt, by = c("coupon_upc", "campaign"))
coupon_product_redeem %>% group_by(sub_commodity_desc) %>% count() %>% arrange(-n)
#yogurt

#Question 4:
transaction_data %>%
  left_join(product, by='product_id') %>%
  group_by(commodity_desc, week_no) %>%
  summarize(total_sales_value = sum(sales_value, na.rm=TRUE)) %>%
  arrange(commodity_desc, week_no) %>%
  group_by(commodity_desc) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  select(-week_no) %>%
  mutate(week_indicator = ifelse(row_number() == 1, 'first', 'last')) %>%
  spread(key = week_indicator, value = total_sales_value) %>%
  mutate(growth = (last-first) / first) %>%
  filter(first >= 100) %>%
  arrange(desc(growth))
#Fluid milk
