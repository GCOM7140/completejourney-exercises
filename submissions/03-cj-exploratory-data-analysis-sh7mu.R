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


#' Q1 - Determine median weekly spend per individual (not household) using
#' price_purchase intransactions_prices and household_size in demographics.
 
transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
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


#' Q2 - Building on Question 2, plot median spend per individual by household
#' size.

transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
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


#' Q3 - Are baskets with diapers in them more likely than average to have beer
#' in them, too? Legend has it that placing these two product categories closer
#' together can increase beer sales (Powers 2002). Using the following starter
#' code, calculate lift for the “association rule” that diapers in a basket
#' (i.e., product_type == "BABY DIAPERS") imply that beer is in the basket
#' (i.e., product_type == "BEERALEMALT LIQUORS"). Does the association between
#' these products offer support for the legend?
 



#' Q4 - Using a stacked bar chart that is partitioned by income level (i.e.,
#' income), visualize the total amount of money that households in the Complete
#' Journey Study spent on national-brand products versus private-label products
#' (i.e., brand).