#Juice Laundry 
install.packages(c("devtools","tidyverse"))
library(devtools)
library(tidyverse)
devtools::install_github(repo = "gcom7140/juicelaundry",
                         auth_token = "001e6c3910d1527245661c5119751043aba76b1f")
library(juicelaundry)

#**Question 1**
#Using the transaction dataset, create a new variable called pct_discount that equals how much of a discount customers tend to receive as a percentage of the regular prices that are displayed on The Juice Laundry’s menus (defined as price_charged + discount_given).
transaction %>%
  mutate(
    pct_discount = (discount_given / (price_charged + discount_given)) * 100
  )


#**Question 2**
#Which stores tend to give larger discounts?
transaction_location %>%
  mutate(
    pct_discount = (discount_given / (price_charged + discount_given)) * 100
  ) %>% 
  filter(is.finite(pct_discount)) %>%
  group_by(location_name) %>%
  summarize(avg_discount = mean(pct_discount)) %>%
  arrange(desc(avg_discount))
#Top five restaurants give larger discounts are: The Corenr, Preston Ave. Purvelo, The Yeards, Crozet Market


#**Question 3**
#What if we condition on the presence of a discount? Does the answer change?
transaction_location %>%
  mutate(
    pct_discount = (discount_given / (price_charged + discount_given)) * 100
  ) %>% 
  filter(pct_discount > 0) %>%
  group_by(location_name) %>%
  summarize(avg_discount = mean(pct_discount)) %>%
  arrange(desc(avg_discount))
#Yes. Preston Ave will have the highest average discount, followed by Piurvelo, the corner, crozet market and the yard



#**Question 4**
#Create a table to display the minimum, maximum, mean, and standard deviation of the price customers paid per transaction for transactions that received and did not receive a discount. What do you notice?
transaction %>%
  group_by(discount_given > 0) %>%
  summarize(
    min = min(price_charged),
    max = max(price_charged),
    mean = mean(price_charged),
    sd = sd(price_charged)
  )
# The max pay is $39 less when customer is given discount, and the average is $1.5 less. 


#**Question 5** 
#There are 381,278 rows in the line_item dataset. On how many occasions was a stock keeping unit (SKU) sold for $5 or less, and how many unique SKUs were sold for $5 or less?
line_item %>% 
  filter(price_charged / quantity <= 5) %>% 
  select(sku_id) %>% 
  summarize(
    a = nrow(.),
    b = n_distinct(.)
  )
#47,782, 124

#**Question 6**
#What proportion of The Juice Laundry’s transactions involving the sale of one SKU cost customers $10 or more?
line_item %>% 
  group_by(transaction_id) %>% 
  mutate(n_line_items = n()) %>% 
  ungroup() %>% 
  filter(n_line_items == 1, quantity == 1) %>% 
  summarize(proportion_over_10 = mean(price_charged >= 10))
#37.8%



