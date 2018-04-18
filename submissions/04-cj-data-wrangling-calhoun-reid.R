# Reid Calhoun
# Complete Journey HW 4
# April 17, 2018


library(tidyverse)
library(completejourney)

# use these data sets in complete journey package:
# coupon, coupon_redempt, campaign_table, transaction_data, and product

##################
### Exercise 1 ###
##################

# What percent of households that received the retailer's weekly mailer redeemed at least one coupon?

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))


##################
### Exercise 2 ###
##################


# How many households did not redeem a coupon?
  
left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = sum(is.na(n.y)))



##################
### Exercise 3 ###
##################


# What percent of coupons promoted in the retailer's weekly mailer got redeemed at least once?

left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by = 'coupon_upc'
) %>% 
  summarize(pct_redeemed = mean(!is.na(n.y)))



##################
### Exercise 4 ###
##################

left_join(transaction_data,product) %>% 
  group_by(sub_commodity_desc, week_no) %>%
  summarize(sales_value_category = sum(sales_value, na.rm = TRUE)) %>%
  filter(week_no >= 52 & week_no <= 102) %>%
  mutate(
    revenue_growth = ifelse(lag(sales_value_category) >= 100,
                            (sales_value_category - lag(sales_value_category))/
                              lag(sales_value_category), NA)
    ) %>% 
  arrange(desc(revenue_growth))
  
# Between weeks 52 and 102, revenue for electronic gift cards grew the most of anything in week 92
# the question asked between weeks 52 and 102, yet the answers suggest that the question only asks for two weeks - 52 and 102



