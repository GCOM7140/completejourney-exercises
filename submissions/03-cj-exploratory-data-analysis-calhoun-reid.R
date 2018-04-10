# Reid Calhoun
# Complete Journey HW 3
# April 10, 2018

library(tidyverse)
library(completejourney)

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


##################
### Question 1 ###
##################

#How many unique households exist in my_transaction_data, and how many of these households in my_transaction_data have demographic data in hh_demographic?

my_transaction_data %>% 
  distinct(household_key) %>% 
  nrow()
# 2500 unique households exist in my_transaction_data

inner_join(my_transaction_data, hh_demographic) %>% 
  distinct(household_key) %>% 
  nrow()
# 801 unique households have demographic data in hh_demographic


##################
### Question 2 ###
##################

# Determine median weekly spend per individual using the following tibble 

inner_join(my_transaction_data, hh_demographic) %>% 
  mutate(
    hh_size          = str_replace(household_size_desc, '5\\+', '5') %>% 
      as.integer()
  ) %>% 
  group_by(household_key, week_no) %>% 
  summarize(
    total_spend      = sum(purchase_price, na.rm = TRUE),
    hh_size          = max(hh_size,        na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  mutate(
    wkly_spend_per_ind = total_spend / hh_size
  ) -> exercise_2


median(exercise_2$wkly_spend_per_ind)
# median = 19.425


##################
### Question 3 ###
##################

# Building on Exercise 2, plot median spend per individual for the five household sizes in my_transaction_data


exercise_2 %>% 
  group_by(hh_size) %>% 
  summarize(
    median_spend_per_individual = median(wkly_spend_per_ind), na.rm = TRUE
            ) %>% 
  ggplot(aes(hh_size, median_spend_per_individual)) +
  geom_col()


##################
### Question 4 ###
##################


# Are baskets with diapers in them more likely than average to have beer in them too? 


inner_join(my_transaction_data, product) %>% 
  mutate(
    diapers = sub_commodity_desc == 'BABY DIAPERS', 
    beer    = sub_commodity_desc == 'BEERALEMALT LIQUORS'
  ) %>% 
  group_by(basket_id) %>% 
  summarize(
    basket_has_diapers = max(diapers),
    basket_has_beer = max(beer)
  ) %>% 
  summarize(
    prop_both = sum(basket_has_diapers*basket_has_beer == 1)/
      sum(basket_has_beer == 1),
    prob_beer = mean(basket_has_beer),
    diaper_lift = prob_both/prob_beer
  )
# Yes, diapers help sell beer


##################
### Question 5 ###
##################

#Using a stacked bar chart that's partitioned by income level (i.e., income_desc), visualize the total amount of money customers spent on national-brand products versus private-label products.


inner_join(my_transaction_data, hh_demographic) %>% 
  mutate(
    income_desc = factor(income_desc, 
                         levels = c('Under 15K',   '15-24K',   '25-34K', 
                                    '35-49K',   '50-74K',   '75-99K', 
                                    '100-124K', '125-149K', '150-174K', 
                                    '175-199K', '200-249K',    '250K+'),
                         ordered = TRUE)
  ) %>%
  group_by(income_desc, brand) %>%
  summarize(total_spend = sum(purchase_price)) %>% 
  ggplot() +
  geom_col(aes(x = income_desc, y = total_spend, fill = brand), 
           position = 'fill') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
   



