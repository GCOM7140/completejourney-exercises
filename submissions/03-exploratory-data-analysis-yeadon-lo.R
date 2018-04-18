##################################################
## Project: Explroratory data analyss
## Script purpose: Hoemework sbmission
## Date: April 2018
## Author: Lo Yeadon
##################################################

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

#Excercise 1: How many unique households exist in my_transaction_data, and how many of these households in my_transaction_data have demographic data in hh_demographic?

#a: 2500
my_transaction_data %>% 
  distinct(household_key) %>% 
  nrow()
#b: 801
inner_join(my_transaction_data, hh_demographic) %>% 
  distinct(household_key) %>% 
  nrow()

#Excercise 2: Determine median weekly spend per individual using the following tibble (i.e., exercise_2).

#Answer 2: 19.4
exercise_2 %>% 
  summarize(
    med_wkly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)
  )

#Excercise 3:Building on Exercise 2, plot median spend per individual for the five household sizes in my_transaction_data.

#Answer 3:
exercise_2 %>% 
group_by(hh_size) %>% 
  summarize(
    med_wkly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = hh_size, y = med_wkly_spend_per_ind)) +
  geom_col()

#Exercise 4:Are baskets with diapers in them more likely than average to have beer in them too? Legend has it that placing these two product categories closer together can increase beer sales (Powers 2002). Using the following starter code, calculate lift for the "association rule" that diapers in a basket (i.e., sub_commodity_desc == 'BABY DIAPERS') imply beer is in the basket (i.e., sub_commodity_desc == 'BEERALEMALT LIQUORS'). Is the association between these products practically significant in my_transaction_data?

#Answer 4
#Probbility both:0.0602
#Probability beer: 0.0566
#Probabiliy diaper lift: 1.06


#Excercise 5: Using a stacked bar chart that's partitioned by income level (i.e., income_desc), visualize the total amount of money customers spent on national-brand products versus private-label products.

#Answer 5: 
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