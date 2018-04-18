knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(completejourney)

################################################################################
################################################################################
# Question 1. What percent of households that received the retailer's weekly mailer redeemed at least one coupon?

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y))) ## 27.4%


################################################################################
################################################################################
# Question 2. How many households did not redeem a coupon?

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = sum(is.na(n.y))) ## 1150

################################################################################
################################################################################
# Question 3. What percent of coupons promoted in the retailer's weekly mailer got redeemed at least once?

left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by = 'coupon_upc'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y))) # 49.0%



################################################################################
################################################################################
# Question 4. Using the transaction_data and product datasets, determine which product category (i.e., sub_commodity_desc) grew the most in terms of revenue for the retailer in the second half of the study period (i.e., between week_no == 52 and week_no == 102). Only consider product categories that had over $100 in revenue in week 52, and calculate revenue growth as a percentage of category revenue in week 52.

# Here are some suggested steps:

##Join the transaction_data and product datasets.
##Group the data by sub_commodity_desc and week_no.
##Calculate sales_value at the category level.
##Filter the data to only include category revenues in weeks 52 and 102.
##Create a new variable called revenue_growth, making use of the lag() function. Recognize that you only want to consider categories that had category revenues of at least $100 in week 52.
##Arrange the data in descending order according to revenue_growth.

left_join(transaction_data, product) %>%
  group_by(sub_commodity_desc, week_no) %>%
  summarize(sales_value_category = sum(sales_value, na.rm = TRUE)) %>%
  filter(week_no == 52 | week_no == 102) %>%
  mutate(
    revenue_growth = ifelse(lag(sales_value_category) >= 100,
                            (sales_value_category - lag(sales_value_category)) /
                              lag(sales_value_category), NA)
  ) %>%
  arrange(desc(revenue_growth)) # Select Beef had the greatest revenue growth of 85.2%.