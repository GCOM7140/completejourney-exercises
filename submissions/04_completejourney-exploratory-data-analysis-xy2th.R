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
    
    # Median weekly spend is $44.20.
    
  
    #Q2
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
    
    #Q3
    er is in the basket (i.e., product_type == "BEERALEMALT LIQUORS"). Does the association between these products offer support for the legend?
      
      transactions_prices %>% 
      inner_join(products, by = "product_id") %>% 
      mutate(
        diapers = product_type == "BABY DIAPERS", 
        beer    = product_type == "BEERALEMALT LIQUORS"
      )
    
    
    
    #Q4
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