library(tidyverse)
library(completejourney)
#Question 1#
hh_coupon_counts <- coupon_redempt %>% count(household_key)
hh_campaign_participants <- campaign_table %>% distinct(household_key)
hh_redemption_check<- left_join(hh_campaign_participants,hh_coupon_counts, by='household_key')
mean(!is.na(hh_redemption_check$n))
#[1] 0.2739899#

#Question 2#
hh_coupon_counts<-coupon_redempt %>% count(household_key)
hh_campaign_participants<- campaign_table %>% distinct(household_key)
anti_join(hh_campaign_participants,hh_coupon_counts, by='household_key') %>% nrow()
#[1] 1150

#Question 3#
coupon_data <- inner_join(coupon, 
                          product, by=c('product_id'))
product_redemption_data <- inner_join(coupon_redempt,
                                      coupon_data, by=c('campaign', 'coupon_upc'))

product_redemption_data %>%
  count(sub_commodity_desc) %>%
  arrange(desc(n)) %>%
  filter(row_number() <= 3)

#Question 4#
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