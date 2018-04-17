#Preparing to boot hmwrk...

library(tidyverse)
library(completejourney)

#Completejourney 03 data prep

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

############
#Question 1#
############

#How many unique households exist in my_transaction_data, and how many of these households in my_transaction_data have demographic data in hh_demographic?
  
#Use distinct() to create a tibble of unique household_key values.
#Use nrow() to count these households.
#Use inner_join() to match my_transaction_data with hh_demographic.
#Use distinct() and nrow() to count the rows that remain.


nrow(distinct(my_transaction_data, household_key))
#2500

inner_join(my_transaction_data, hh_demographic) %>%
distinct(household_key) %>%
nrow()
#801

############
#Question 2#
############

#Determine median weekly spend per individual using the following tibble (i.e., exercise_2).

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

#examine the data
str(exercise_2)

#calculate the median
exercise_2 %>%
summarize(weekly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE))
#19.4

############
#Question 3#
############

#Building on Exercise 2, plot median spend per individual for the five household sizes in my_transaction_data.

#Snapping Q2 code
exercise_2 %>%
#Adding group_by here because code won't work without it, not sure why...
group_by(hh_size) %>%
summarize(weekly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)) %>%
#plotting...
ggplot(aes(x = hh_size, y = weekly_spend_per_ind)) +
geom_line()

#Interesting graph.

############
#Question 4#
############

#Are baskets with diapers in them more likely than average to have beer in them too? Legend has it that placing these two product categories closer together can increase beer sales (Powers 2002). Using the following starter code, calculate lift for the "association rule" that diapers in a basket (i.e., sub_commodity_desc == 'BABY DIAPERS') imply beer is in the basket (i.e., sub_commodity_desc == 'BEERALEMALT LIQUORS'). Is the association between these products practically significant in my_transaction_data?
  
#importing Q4 code...
#Running this code lets us see that diapers/beer are either true/false, which we will need to know later
inner_join(my_transaction_data, product) %>% 
mutate(diapers = sub_commodity_desc == 'BABY DIAPERS', 
       beer = sub_commodity_desc == 'BEERALEMALT LIQUORS') %>%
select(diapers)

#Whew, this question was tough to code, had to get some big help from the solutions
#First the Q4 code
inner_join(my_transaction_data, product) %>% 
mutate(diapers = sub_commodity_desc == 'BABY DIAPERS', 
       beer    = sub_commodity_desc == 'BEERALEMALT LIQUORS') %>%
#This is the part I really didn't get, not 100% sure why we have to use code to draw the max
#when the max is going to be a 1? Anyway, got help from solutions here, moving on...
group_by(basket_id) %>%
summarize(basket_has_diapers = max(diapers), 
          basket_has_beer    = max(beer)) %>%
#This next code calucluates lift based off the formula linked on github/verified with solutions
summarize(prop_both   = sum(basket_has_diapers * basket_has_beer == 1) / sum(basket_has_diapers == 1),
          prob_beer   = mean(basket_has_beer),
          diaper_lift = prop_both / prob_beer)

#So, it looks like the probability of both beer and diapers is 6% higher than the probability of beer alone. Not a significant increase, could just be random chance at that low, but I suppose if you wanted to eek out every penny (aka walmart perhaps) then maybe this would be an okay assumption to roll with.

############
#Question 4#
############

#Using a stacked bar chart that's partitioned by income level (i.e., income_desc), visualize the total amount of money customers spent on national-brand products versus private-label products.

#Let's check out this data
inner_join(my_transaction_data, hh_demographic) %>% 
select(income_desc)
#We know income is character data and has to be converted
#Now the factoring, got some big help from solutions here because I kept crashing R with my code somehow...
inner_join(my_transaction_data, hh_demographic) %>% 
mutate(income_desc = factor(income_desc, levels = c('Under 15K',   '15-24K',   '25-34K', 
'35-49K',   '50-74K',   '75-99K', 
'100-124K', '125-149K', '150-174K', 
'175-199K', '200-249K',    '250K+'),
ordered = TRUE)
) %>%
group_by(income_desc, brand) %>%
summarize(total_spend = sum(purchase_price)) %>% 
#Now we do the plotting
ggplot() +
geom_col(aes(x = income_desc, y = total_spend, fill = brand), 
position = 'fill') +
#Above creates the histogram, but the x axis is impossible to read. Next part makes it a real "visual"
theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Exercise 3 complete!