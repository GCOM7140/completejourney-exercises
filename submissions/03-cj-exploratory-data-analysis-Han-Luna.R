#' ---
#' title: "Answers to the CJ EDA Exercise"
#' author: Luna Han
#' date: April 10, 2018
#' output: github_document
#' ---
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
str(my_transaction_data)

#Exercise 1
#How many unique households exist in my_transaction_data, and how many of these households in my_transaction_data have demographic data in hh_demographic?
my_transaction_data %>%
  distinct(household_key) %>%
  nrow()

inner_join( my_transaction_data, hh_demographic) %>%
  distinct() %>%
  nrow()

#Exercise 2
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

mutate(exercise_2,
       md_wkly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)
)

#Exercise 3
#Building on Exercise 2, plot median spend per individual for the five household sizes in my_transaction_data.
exercise_2 %>%
  group_by(hh_size) %>%
  mutate(
    md_wkly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = hh_size, y = md_wkly_spend_per_ind)) +
  geom_col()

#Exercise 4
#Are baskets with diapers in them more likely than average to have beer in them too? Legend has it that placing these two product categories closer together can increase beer sales (Powers 2002). Using the following starter code, calculate lift for the "association rule" that diapers in a basket (i.e., sub_commodity_desc == 'BABY DIAPERS') imply beer is in the basket (i.e., sub_commodity_desc == 'BEERALEMALT LIQUORS'). Is the association between these products practically significant in my_transaction_data?
exercise_4 <- inner_join(my_transaction_data, product) %>% 
  mutate(
    diapers = sub_commodity_desc == 'BABY DIAPERS', 
    beer    = sub_commodity_desc == 'BEERALEMALT LIQUORS'
  ) 
  exercise_4 %>%
    group_by(basket_id) %>%
    summarize(
      with_diapers = max(diapers), 
      with_beer    = max(beer)
    ) %>% 
    summarize(
      prob_together= sum(with_diapers * with_beer == 1) / 
        sum(with_diapers == 1),
      prob_beer    = mean(with_beer),
      lift_db      = prob_together / prob_beer
    )
    
#Exercise 5 Using a stacked bar chart that's partitioned by income level (i.e., income_desc), visualize the total amount of money customers spent on national-brand products versus private-label products.
  inner_join(my_transaction_data, hh_demographic) %>% 
    mutate(
      income_group = factor(income_desc, 
                           levels = c('Under 10K',   '11-20K',   '21-30K', 
                                      '31-40K',   '41-70K',   '71-90K', 
                                      '91-120K', '121-140K', '141-170K', 
                                      '171-200K', '201-249K',    '250K+'),
                           ordered = TRUE)
    ) %>%
    group_by(income_group, brand) %>%
    mutate  (total_spend = sum(purchase_price)) %>% 
    ggplot() +
    geom_col(aes(x = income_group, y = total_spend, fill = brand), 
             position = 'fill') 

  