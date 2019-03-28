#' title: "Exploratory Data Analysis (EDA) Exercises and Answers"
#' author: McLean Long (jml7cm)
#' output: github_document

# Remember to load libraries before starting

library(tidyverse)
library(completejourney)

# Before beginning you have to make 'transaction_prices' or you will have a number of issues. 

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

# Now that the data frame 'transactions_prices' has been created the exercises can be undertaken

### Question 1 ### - multipart 

# Part 1

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

# This is quite a bit of code, but it is used to find the median weekly spending value = $44.20

### Question 2 ###

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

# The correct code for Q2 produces a really nice bar chart that allows for further study based on weekly spend vs. household size and other variables.

### Question 3 ###

transactions_prices %>% 
  inner_join(products, by = "product_id") %>% 
  mutate(
    diapers = product_type == "BABY DIAPERS", 
    beer    = product_type == "BEERALEMALT LIQUORS"
  )

# two different strings of code, so that it is easy to see the initial code and then the deeper dive to study the potential relationship that is being studied in this question. 

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

# I was not sure initially how to interpret this output, so I will need to check back or continue to study this code in case my team needs something like this for the final TJL project.

### Question 4 ###

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

# I thought this was a really cool graphic, so I ended up saving it. Considering that the CJ data is somewhat comparable to the TJL data - many of the visualizations that work for CJ might be useful for the final project for TJL as well. 

### End of code for Exercises #3. Picks back up for Data Wrangling #4 ###


