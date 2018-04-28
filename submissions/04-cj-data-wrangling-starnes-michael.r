#Michael Starnes
#GCOM 7140
#Professor Boichuck


library(tidyverse)
library(completejourney)

#Question 1
left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))

#about 28% of families who were mailed a coupon used it



#Question 2: Counting Number of Families


left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate=sum(is.na(n.y)))

#there are 1150 families that never tried coupons





#Question 3

left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by ='coupon_upc'
) %>% 
  summarize(redemption_rate=mean(!is.na(n.y)))

#About half(49%)




#Question 4: Finding largest product category growth
left_join(transaction_data, product) %>%
  
  group_by(sub_commodity_desc, week_no) %>%
  
  summarize(sales_value_category = sum(sales_value, na.rm = TRUE)) %>%
  filter(week_no == 52 | week_no ==102) %>%
  mutate(
    revenue_growth = ifelse(lag(sales_value_category) >= 100,
                            (sales_value_category - lag(sales_value_category)) /
                              lag(sales_value_category), NA)
  ) %>%
  
  
  arrange(desc(revenue_growth))
#select beef had the highest amount of revenue growth, at 85.2%