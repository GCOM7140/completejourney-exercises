#' ---
#' title: "CJ Exercise #3"
#' author: Dominic Sfreddo
#' date: April 10, 2018
#' output: github_document
#' ---

knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(completejourney)

my_transaction_data <- left_join(transaction_data, 
                                 product, by='product_id')
my_transaction_data <- left_join(my_transaction_data, 
                                 hh_demographic, by='household_key')
my_campaign_table <- left_join(campaign_table,
                               campaign_desc, by=c('campaign','description'))

#1

my_transaction_data %>% 
  distinct(household_key) %>% 
  inner_join(hh_demographic, by = 'household_key') %>% 
  nrow()

#2

avg_weekly_spent <- my_transaction_data %>% 
  filter(household_size_desc != 'na') %>% 
  mutate(hh_person_count = as.integer(gsub('\\+' ,'', household_size_desc))) %>% 
  group_by(household_key, week_no) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE),
            total_person = max(hh_person_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(weekly_spent_per_person = total_sales_value/total_person)

avg_weekly_spent %>% 
  summarize(weekly_avg_spent_per_person = mean(weekly_spent_per_person))

#3

avg_weekly_spent_demo <- left_join(avg_weekly_spent, hh_demographic, by = "household_key")

avg_weekly_spent_demo %>% 
  group_by(kid_category_desc) %>%
  summarise(weekly_avg_spent_per_person = mean(weekly_spent_per_person)) %>% 
  ggplot(aes(kid_category_desc, weekly_avg_spent_per_person)) +
  geom_bar(stat = 'identity')

#4

my_transaction_data %>%
  filter(!is.na(kid_category_desc), grepl('BABY', sub_commodity_desc)) %>% 
  mutate(hh_kid_count = as.integer(gsub('\\+' ,'', kid_category_desc))) %>% 
  group_by(household_key) %>% 
  summarise(total_sales_value = sum(sales_value, na.rm = TRUE),
            avg_kid_count = mean(hh_kid_count, na.rm = TRUE)) %>%
  ggplot(aes(avg_kid_count, total_sales_value)) +
  geom_point() +
  geom_smooth()

 #There's no correlation between total sales on baby products and number of kids.

#5

my_transaction_data %>% 
  select(basket_id, sub_commodity_desc) %>% 
  filter(!is.na(sub_commodity_desc)) %>% 
  mutate(beer_indicate = (sub_commodity_desc == 'BEERALEMALT LIQUORS'),
         diaper_indicate = (sub_commodity_desc == 'BABY DIAPERS')) %>%
  group_by(basket_id) %>%
  summarise(basket_beer_indicate = any(beer_indicate),
            basket_diaper_indicate = any(diaper_indicate)) %>%
  mutate(diapers_and_beer = basket_beer_indicate * basket_diaper_indicate) %>%
  summarise(pct_beer_and_diaper = mean(diapers_and_beer))

#6

brand_data$income_desc

brand_data <- my_transaction_data %>% 
  filter(!is.na(income_desc), !is.na(brand)) %>% 
  group_by(income_desc, brand) %>% 
  summarise(total_sales_value = sum(sales_value))

ggplot(brand_data, aes(income_desc, total_sales_value)) +
  geom_bar(aes(fill = brand), stat = 'identity', position = 'fill') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#7

coupon_matched <- transaction_data %>% 
  select(coupon_match_disc, coupon_disc) %>% 
  filter(coupon_disc < 0,
         coupon_match_disc < 0)

coupon_used <- transaction_data %>% 
  select(coupon_match_disc, coupon_disc) %>% 
  filter(coupon_disc < 0)

nrow(coupon_matched)/nrow(coupon_used)

