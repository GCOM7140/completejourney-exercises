#Homework Set for March 28 

#Libraries 

library(tidyverse)
library(completejourney)

#Create Transaction Prices 

transactions %>% 
  filter(quantity != 0) %>%
  mutate(
    price_regular  = (sales_value + retail_disc + coupon_match_disc) /
      quantity,
    price_loyalty  = (sales_value + coupon_match_disc) / 
      quantity,
    price_coupon   = (sales_value - coupon_disc) / 
      quantity,
    price_purchase = case_when(
      coupon_disc > 0 ~ price_coupon, 
      retail_disc > 0 ~ price_loyalty,
      TRUE            ~ price_regular
    )
  ) -> 
  transactions_prices

#Question One - Determine median weekly spend per individual (not household) using price_purchase in transaction_prices and household_size in demographics 

transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
      as.integer()
  )
transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size     = str_replace(household_size, "5\\+", "5") %>% 
      as.integer()
  ) %>% 
  group_by(household_id, week) %>%
  mutate(
    spend_total        = sum(price_purchase, na.rm = TRUE),
    spend_wkly_per_ind = spend_total / household_size
  ) %>% 
  ungroup() %>% 
  summarize(
    spend_wkly_per_ind_med = median(spend_wkly_per_ind, na.rm = TRUE)
    
  )

## # A tibble: 1 x 1
##   spend_wkly_per_ind_med
##                    <dbl>
## 1                   44.2

#median weekly spend is $44.20 

#Question Two - plot median spend per individual by household size 

transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size     = str_replace(household_size, "5\\+", "5") %>% 
      as.integer()
  ) %>% 
  group_by(household_id, week) %>%
  mutate(
    spend_total        = sum(price_purchase, na.rm = TRUE),
    spend_wkly_per_ind = spend_total / household_size
  ) %>% 
  group_by(household_size) %>% 
  summarize(
    spend_wkly_per_ind_med = median(spend_wkly_per_ind, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = household_size, y = spend_wkly_per_ind_med)) +
  geom_col()

#These graphs show to us that the median weekly spend per individual goes down as household size increases - for example the median spend for the 1 person household is $80, then for 2 it's $40, so we can see that having more people in your house means you're going to likely spend lesson an individual basis. 

#Question Three - Are baskets with diapers in them more likely than average to have beer in them, too? 

transactions_prices %>% 
  inner_join(products, by = "product_id") %>% 
  mutate(
    diapers = product_type == "BABY DIAPERS", 
    beer    = product_type == "BEERALEMALT LIQUORS"
  ) %>% 
  group_by(basket_id) %>%
  summarize(
    basket_has_diapers = max(diapers), 
    basket_has_beer    = max(beer)
  ) %>% 
  summarize(
    prop_both   = sum(basket_has_diapers * basket_has_beer == 1, na.rm = TRUE) 
    / sum(basket_has_diapers == 1, na.rm = TRUE),
    prob_beer   = mean(basket_has_beer, na.rm = TRUE),
    diaper_lift = prop_both / prob_beer
  ) 
    ## # A tibble: 1 x 3
    ##   prop_both prob_beer diaper_lift
    ##       <dbl>     <dbl>       <dbl>
    ## 1    0.0552    0.0554       0.996

#We do not see that there is a notably higher probability of beer and diapers being purchased together. 


#Question Four - Using a stacked bar chart that is partitioned by income level (i.e., income), visualize the total amount of money that households in the Complete Journey Study spent on national-brand products versus private-label products (i.e., brand).

transactions_prices %>% 
  left_join(demographics, by = "household_id") %>% 
  left_join(products, by = "product_id")

transactions_prices %>% 
  left_join(demographics, by = "household_id") %>% 
  left_join(products, by = "product_id") %>% 
  group_by(income, brand) %>%
  summarize(spend_total = sum(price_purchase)) %>% 
  ggplot(mapping = aes(x = income, y = spend_total, fill = brand)) +
  geom_col(position = "fill") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#see the graph to show that households spent more on nationl brands across income brackets as compared to private brands. 