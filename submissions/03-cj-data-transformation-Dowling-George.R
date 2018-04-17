#' George Dowling HW#3
#' Customer Analytics
#' April 10, 2018

library(tidyverse)
library(completejourney)
library(ggplot2)

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

####' Question 1 ####

my_transaction_data %>%
  distinct(household_key) %>%
  nrow()

#' 2500 unique household_key values

inner_join(my_transaction_data, hh_demographic) %>%
  distinct(household_key) %>%
  nrow()

#' 801 


####' Question 2 ####

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

exercise_2 %>%
  summarise(median_week_spend = median(wkly_spend_per_ind))

#' 19.4


####' Question 3 ####

exercise_2 %>%
  group_by(hh_size) %>%
  summarise(median_week_spend = median(wkly_spend_per_ind)) %>%
  ggplot(aes(hh_size, median_week_spend)) + 
  geom_bar(stat = "identity", fill = "LightBlue", color = "Black")


####' Question 4 ####

inner_join(my_transaction_data, product) %>% 
  mutate(
    diapers = sub_commodity_desc == 'BABY DIAPERS', 
    beer    = sub_commodity_desc == 'BEERALEMALT LIQUORS'
  ) %>%
  group_by(basket_id) %>%
  summarise(basket_diapers = max(diapers), 
            basket_beer = max(beer)) %>%
  summarise(
    prop_both = sum(basket_diapers * basket_beer == 1) / sum(basket_diapers == 1),
    prop_beer = mean(basket_diapers),
    diaper_lift = prop_both / prop_beer
  )


####' Question 5 #### 

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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()




