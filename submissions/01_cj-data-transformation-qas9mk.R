install.packages("tidyverse")
library(tidyverse)
df <- data(transaction) 

#Question 1
transaction$pct_discount <- discount_given / (price_charged + discount_given)) * 100

#Question 2

transaction_location <- left_join(transaction, location, by = "location_id")

transaction_location %>%
  mutate(
    pct_discount = (discount_given / (price_charged + discount_given)) * 100
  ) %>% 
  filter(is.finite(pct_discount)) %>%
  group_by(location_name) %>%
  summarize(avg_discount = mean(pct_discount)) %>%
  arrange(desc(avg_discount))

#Question 3

#pass

#Question 4

transaction %>%
  group_by(discount_given > 0) %>%
  summarize(
    min = min(price_charged),
    max = max(price_charged),
    mean = mean(price_charged),
    sd = sd(price_charged)
  )

#nothing very notable

#Question 5

line_item %>% 
  filter(price_charged / quantity <= 5) %>% 
  select(sku_id) %>% 
  summarize(
    a = nrow(.))

#Question 6

line_item %>% 
  group_by(transaction_id) %>% 
  mutate(n_line_items = n()) %>% 
  ungroup() %>% 
  filter(n_line_items == 1, quantity == 1) %>% 
  summarize(proportion_over_10 = mean(price_charged >= 10))

#about 39% of transactions