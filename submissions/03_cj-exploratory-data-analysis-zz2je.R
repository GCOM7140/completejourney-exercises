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


#Question 1: Determine median weekly spend per individual using the following tibble
transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
      as.integer()
  ) %>% 
  group_by(household_id, week) %>%
  mutate(
    total_spending = sum(price_purchase, na.rm = TRUE),
    spend_weekly_per_ind = total_spending / household_size
  ) %>% 
  ungroup() %>% 
  summarize(
    median_spend_weekly_per_ind = median(spend_weekly_per_ind, na.rm = TRUE)
  )

# Thus, median weekly spend per individual is $44.2


#Question 2: Building on Question 2, plot median spend per individual for the five household sizes in my_transaction_data.
transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
      as.integer()
  ) %>% 
  group_by(household_id, week) %>%
  mutate(
    total_spending = sum(price_purchase, na.rm = TRUE),
    spend_weekly_per_ind = total_spending / household_size
  ) %>% 
  group_by(household_size) %>% 
  summarize(
    median_spend_weekly_per_ind = median(spend_weekly_per_ind, na.rm = TRUE)
  ) %>% 
  ggplot()+
  geom_bar(mapping = aes(x= household_size, y=median_spend_weekly_per_ind),stat ="identity")


#Question 3: Are baskets with diapers in them more likely than average to have beer in them too? 
transactions_prices %>% 
  inner_join(products, by = "product_id") %>% 
  mutate(
    diapers = product_type == "BABY DIAPERS", 
    beer    = product_type == "BEERALEMALT LIQUORS"
  ) %>% 
  group_by(basket_id) %>% 
  summarise(basket_with_diapers = max(diapers), 
            basket_with_beer = max(beer)) %>% 
  summarize(
    prop_both   = sum(basket_with_diapers *basket_with_beer == 1,na.rm = TRUE) 
    / sum(basket_with_diapers == 1, na.rm = TRUE),
    prob_beer   = mean(basket_with_beer, na.rm = TRUE),
    diaper_lift = prop_both / prob_beer
  )

#     prop_both prob_beer diaper_lift
#     0.0552    0.0554       0.996


#Question 5: Using a stacked bar chart that's partitioned by income level (i.e., income), visualize the total amount of money customers spent on national-brand products versus private-label products. 

transactions_prices %>% 
  left_join(demographics, by = "household_id") %>% 
  left_join(products, by = "product_id") %>% 
  group_by(income,brand) %>% 
  summarise(total_amount_cus_spent = sum(price_purchase)) %>% 
  ggplot()+
  geom_bar(mapping = aes(x=income, y=total_amount_cus_spent,fill=brand ), stat= "identity",position = "fill")+
  theme(axis.text.x = element_text(angle = 30, vjust = 1))
 

