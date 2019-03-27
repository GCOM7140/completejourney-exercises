# Exploratory Data Analysis

library(tidyverse)
library(completejourney)

transactions_prices <- transactions %>% 
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
  )

# Question 1
transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
      as.integer()
  ) %>%
  group_by(household_id, week) %>%
  mutate(
    spend_total = sum(price_purchase, na.rm = TRUE),
    spend_weekly_per_indiv = spend_total / household_size
  ) %>%
  ungroup() %>%
  summarise(
    spend_weekly_per_indiv_median = median(spend_weekly_per_indiv, na.rm = TRUE)
  )

# 44.2

# Question 2
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
  ggplot(mapping = aes(x = household_size, y = spend_wkly_per_ind_med)) +
  geom_col()

# Question 3
transactions_prices %>% 
  inner_join(products, by = "product_id") %>% 
  mutate(
    diapers = product_type == "BABY DIAPERS", 
    beer    = product_type == "BEERALEMALT LIQUORS"
  ) %>%
  group_by(basket_id) %>%
  summarise(
    basket_diapers = max(diapers),
    basket_beer = max(beer)
  ) %>%
  summarise(
    prob_both = sum(basket_diapers * basket_beer == 1, na.rm = TRUE) / sum(basket_diapers == 1, na.rm = TRUE),
    prob_beer = mean(basket_beer, na.rm = TRUE),
    diaper_lift = prob_both / prob_beer
  )

# We do not see a correlation

# Question 4
transactions_prices %>% 
  left_join(demographics, by = "household_id") %>% 
  left_join(products, by = "product_id") %>%
  group_by(income, brand) %>%
  summarise(spend_total = sum(price_purchase)) %>%
  ggplot(mapping = aes(x = income, y = spend_total, fill = brand)) +
  geom_col(position = "fill")
