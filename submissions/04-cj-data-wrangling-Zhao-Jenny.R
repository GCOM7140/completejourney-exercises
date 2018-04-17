# Jenny Zhao
# April 17

knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(completejourney)

# Question 1

#distinct()

left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))
# 27.4% of households that received the retailer's weekly mailer redeemed at least one coupon.

# Question 2
#anti_join()
left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = sum(is.na(n.y)))
# 1150 households did not redeem a coupon.

# Question 3
coupon_redempt
sub_commodity_desc
inner_join()

left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by = 'coupon_upc'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))
# 49% of coupons promoted in the retailer's weekly mailer got redeemed at least once.

# Question 4
commodity_desc
group_by()
summarize()
sales_value
commodity_desc
week_no
filter(row_number() == 1 | row_number() == n())
ifelse(row_number() == 1, 'first', 'last')
week_no
spread()
(last - first)/first
filter()
arrange()

left_join(transaction_data, product) %>%
  group_by(sub_commodity_desc, week_no) %>%
  summarize(sales_value_category = sum(sales_value, na.rm = TRUE)) %>%
  filter(week_no == 52 | week_no == 102) %>%
  mutate(
    revenue_growth = ifelse(lag(sales_value_category) >= 100,
                            (sales_value_category - lag(sales_value_category)) /
                              lag(sales_value_category), NA)
  ) %>%
  arrange(desc(revenue_growth))
# "select beef" grew the most in terms of revenue for the retailer in the second half of the study period. 












