#Prepping libraries

library(tidyverse)
library(completejourney)

#Use the coupon, coupon_redempt, campaign_table, transaction_data, and product datasets
############
#Question 1#
############

#Couldn't figure out how to do this question at all so tried to disect the solution and understand how it was figured out.

#What percent of households that received the retailer's weekly mailer redeemed at least one coupon?
?coupon_redempt
?campaign_table
#After taking a look at the tables, we know we have to do a join to get the answer we need
Q1table <- left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key') %>%
  #Q1table
#After looking at the new table, we know variable names and can calculate the percentage
  summarize(percent_households_redeemed = mean(!is.na(n.y)))

############
#Question 2#
############

#How many households did not redeem a coupon?

#Now that I've seen how it's done in question 1, this question is easy.
#First we join tables at household key and get counts.
left_join(campaign_table %>% count(household_key),
                     coupon_redempt %>% count(household_key),
                     by = "household_key"
                     ) %>%
#Now we can just count the number of households with unredeemed coupons.
  summarize(total_customers_that_didnt_redeem = sum(is.na(n.y)))
#Would be nice to be able to use count function within summarize, but I couldn't get it to work, in this case sum works fine.


############
#Question 3#
############

#What percent of coupons promoted in the retailer's weekly mailer got redeemed at least once?
str(coupon)
#This question follows the same logic as above
left_join(coupon %>% count(coupon_upc),
                     coupon_redempt %>% count(coupon_upc),
                     by = "coupon_upc") %>%
  summarize(percent_mailer_coupons_redeemed = mean(!is.na(n.y)))

############
#Question 4#
############

#Using the transaction_data and product datasets, determine which product category (i.e., sub_commodity_desc) grew the most in terms of revenue for the retailer in the second half of the study period (i.e., between week_no == 52 and week_no == 102). Only consider product categories that had over $100 in revenue in week 52, and calculate revenue growth as a percentage of category revenue in week 52.

#Here are some suggested steps:
  
  #Join the transaction_data and product datasets.
#Group the data by sub_commodity_desc and week_no.
#Calculate sales_value at the category level.
#Filter the data to only include category revenues in weeks 52 and 102.
#Create a new variable called revenue_growth, making use of the lag() function. Recognize that you only want to consider categories that had category revenues of at least $100 in week 52.
#Arrange the data in descending order according to revenue_growth.

left_join(transaction_data, product) %>%
group_by(sub_commodity_desc, week_no) %>%
summarize(sales_value_by_category = sum(sales_value)) %>%
filter(week_no == 52 | week_no == 102) %>%
#Got stuck on this next part, called in the cavalry
mutate( revenue_growth = ifelse(lag(sales_value_by_category) >= 100, #This sets up the minimum $100 sales revenue, didn't click to use ifelse here for me
                            (sales_value_by_category - lag(sales_value_by_category)) /
                              lag(sales_value_by_category), NA) 
  ) %>%
  arrange(desc(revenue_growth))
#As we can see, SELECT BEEF grew the most of any category