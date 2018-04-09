# Sam Kotz
# Complete Journey questions
library(completejourney)
library(tidyverse)
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

#1
unique_households <- my_transaction_data %>% 
  distinct(household_key)
unique_households %>% nrow()

#there are 2500 unique households in the dataset
inner_join(unique_households, hh_demographic, by='household_key') %>%
  nrow()
#only 801 of those households have data in the hh_demographic dataset

#2
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
  summarize(
    med_wkly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)
  )


#about $19.4

#3
exercise_2 %>% 
  group_by(hh_size) %>% 
  summarize(
    med_wkly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = hh_size, y = med_wkly_spend_per_ind)) +
  geom_col()

#4
inner_join(my_transaction_data, product) %>% 
  mutate(
    diapers = sub_commodity_desc == 'BABY DIAPERS', 
    beer    = sub_commodity_desc == 'BEERALEMALT LIQUORS'
  ) %>%
  group_by(basket_id) %>%
  summarize(
    basket_has_diapers = max(diapers), 
    basket_has_beer    = max(beer)
  ) %>% 
  summarize(
    prop_both   = sum(basket_has_diapers * basket_has_beer == 1) / 
      sum(basket_has_diapers == 1),
    prob_beer   = mean(basket_has_beer),
    diaper_lift = prop_both / prob_beer
  )
#The lift from each product is extremely small, so it does not look like the relationship between the two products is practically significant. 

#5
brand_stats <- my_transaction_data %>% 
  filter(!is.na(income_desc), !is.na(brand)) %>%
  mutate(income_desc = factor(income_desc, 
                              levels=c("Under 15K", "15-24K", "25-34K", "35-49K","50-74K", "75-99K", "100-124K", "125-149K", "150-174K", "175-199K", "200-249K", "250K+"), ordered=TRUE)) %>%
  group_by(income_desc, brand) %>%
  summarize(total_sales_value = sum(sales_value))


ggplot(brand_stats) + 
  geom_bar(mapping = aes(x=income_desc, y=total_sales_value, fill=brand), 
           stat='identity', position='fill') + 
  theme(axis.text.x=element_text(angle=45, hjust=1))