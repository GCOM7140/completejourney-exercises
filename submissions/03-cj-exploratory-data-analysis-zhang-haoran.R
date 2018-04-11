#' ---
#' title: "CJ exploratory data analysis"
#' author: Haoran Zhang
#' date: April 9, 2018
#' output: github_document
#' ---

library(tidyverse)
library(completejourney)

my_transaction_data <- left_join(transaction_data, 
                                 product, by='product_id')
my_transaction_data <- left_join(my_transaction_data, 
                                 hh_demographic, by='household_key')
my_campaign_table <- left_join(campaign_table,
                               campaign_desc, by=c('campaign','description'))

#Question 1:
householdkey <- as.tibble(unique(my_transaction_data$household_key))
colnames(householdkey) <- "household_key"
nrow(householdkey)

inner_join(householdkey, hh_demographic, by = "household_key") %>%
  nrow()
#2500 and 801


#Question 2:
weekly_spend_per_person <- my_transaction_data %>%
  filter(!is.na(household_size_desc)) %>%
  mutate(hh_person_count = as.integer(gsub('\\+' ,'', household_size_desc))) %>%
  group_by(household_key, week_no) %>%
  summarize(person_count = max(hh_person_count, na.rm=TRUE), 
            total_sales_value = sum(sales_value, na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(weekly_spend_per_person = total_sales_value / person_count)

weekly_spend_per_person %>%
  summarize(avg_wkly_spend_per_person = mean(weekly_spend_per_person, na.rm=TRUE))

#Question 3:
weekly_spend_per_person %>% 
  group_by(person_count) %>% 
  summarize(avg_spend_per_person_week = 
              mean(weekly_spend_per_person, na.rm=TRUE)) %>%
  ggplot() + 
  geom_bar(aes(x=person_count, 
               y=avg_spend_per_person_week, 
               fill=person_count), stat='identity', col = "black", alpha = .9) +
          theme_bw()

#Question 4:
my_transaction_data %>%
  filter(!is.na(kid_category_desc), grepl('BABY', sub_commodity_desc)) %>%
  mutate(hh_kid_count = as.integer(gsub('\\+' ,'', kid_category_desc))) %>%
  group_by(household_key) %>%
  summarize(avg_kid_count = mean(hh_kid_count, na.rm=TRUE), 
            total_sales_value = sum(sales_value, na.rm=TRUE)) %>% 
  ggplot(data=., mapping=aes(x=avg_kid_count, y=total_sales_value)) + 
  geom_jitter(width = .05, alpha = .8) + 
  geom_smooth() + theme_bw()

#Question 5:
my_transaction_data %>%
  select(basket_id, sub_commodity_desc) %>%
  filter(!is.na(sub_commodity_desc)) %>%
  mutate(diapers_indicator = (sub_commodity_desc == 'BABY DIAPERS'), 
         beer_indicator = (sub_commodity_desc == 'BEERALEMALT LIQUORS')) %>%
  group_by(basket_id) %>%
  summarize(basket_diapers_indicator = any(diapers_indicator), 
            basket_beer_indicator = any(beer_indicator)) %>%
  mutate(diapers_and_beer = basket_diapers_indicator * basket_beer_indicator) %>%
  ungroup() %>%
  summarize(diapers_and_beer_pct_of_total = mean(diapers_and_beer))


#Question 6:
brand_stats <- my_transaction_data %>% 
  filter(!is.na(income_desc), !is.na(brand)) %>%
  mutate(income_desc = factor(income_desc, 
                              levels=c("Under 15K", "15-24K", "25-34K", "35-49K", 
                                       "50-74K", "75-99K", "100-124K", "125-149K", 
                                       "150-174K", "175-199K", "200-249K", "250K+"), 
                              ordered=TRUE)) %>%
  group_by(income_desc, brand) %>%
  summarize(total_sales_value = sum(sales_value))

ggplot(brand_stats) + 
  geom_bar(mapping = aes(x=income_desc, y=total_sales_value, fill=brand), 
           stat='identity', position='fill') +
  coord_flip()

#Lower incomes buy more private label items


#Question 7:
transaction_data %>%
  select(coupon_match_disc, coupon_disc) %>%
  filter(coupon_disc < 0) %>%
  summarize(coupon_match_rate = mean(coupon_match_disc < 0))
