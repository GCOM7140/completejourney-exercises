library(tidyverse)
library(completejourney)

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

#Question 1: Determine median weekly spend per individual (not household) using price_purchase ransactions_prices and household_size in demographics.


transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
      as.integer()
  ) %>% 
  group_by(household_id, week) %>%
  mutate(total_spending = sum(price_purchase), 
         individual_spd = total_spending / household_size) %>%
  ungroup() %>%
  summarize(median_wkly_spd = median(individual_spd))
  

#Question 2: Building on Question 2, plot median spend per individual by household size.

transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
      as.integer()
  ) %>% 
  group_by(household_id) %>%
  mutate(total_spending = sum(price_purchase), 
         individual_spd = total_spending / household_size) %>%
  group_by(household_size) %>%
  summarize(median = median(individual_spd)) %>%
  ggplot(mapping = aes(x = household_size, y = median)) +
  geom_col()

#Question 3: Are baskets with diapers in them more likely than average to have beer in them, too? Legend has it that placing these two product categories closer together can increase beer sales (Powers 2002). Using the following starter code, calculate lift for the “association rule” that diapers in a basket (i.e., product_type == "BABY DIAPERS") imply that beer is in the basket (i.e., product_type == "BEERALEMALT LIQUORS"). Does the association between these products offer support for the legend?
  
  transactions_prices %>% 
  inner_join(products, by = "product_id") %>% 
  mutate(
    diapers = product_type == "BABY DIAPERS", 
    beer    = product_type == "BEERALEMALT LIQUORS" 
  ) %>% #filter prodcut type and also mutate two logical columns one named diapers and the other named beer.
  group_by(basket_id) %>%
  summarize(
    basket_with_diapers = max(diapers),
    basket_with_beer = max(beer) #turns logical into integer
    ) %>%
    ungroup %>%
  mutate(
    prob_both = sum(basket_with_diapers + basket_with_beer == 2, na.rm = TRUE)/sum(basket_with_diapers == 1, na.rm = TRUE),
    prob_beer = mean(basket_with_beer, na.rm = TRUE)
    ) %>% 
  summarize(product_lift = mean(prob_both / prob_beer))
#The probability of buying beer and diapers together is 99.6%, the myth is real. 
  
  
#Question 4: Using a stacked bar chart that is partitioned by income level (i.e., income), visualize the total amount of money that households in the Complete Journey Study spent on national-brand products versus private-label products (i.e., brand).

transactions_prices %>% 
  left_join(demographics, by = "household_id") %>% 
  left_join(products, by = "product_id") %>%
  group_by(income, brand) %>%
  filter(income != "NA", brand != "NA") %>%
  summarize(purchase = sum(price_purchase, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = income, y = price_purchase), fill = brand) +
  geom_col(position = "fill")
  

