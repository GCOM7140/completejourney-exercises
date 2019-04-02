library(tidyverse)
library(completejourney)
library(lubridate)

#Question 1: What percent of households that received the retailer’s weekly mailer redeemed at least one coupon?
left_join(
  campaigns          %>% count(household_id, name = "num_received"),
  coupon_redemptions %>% count(household_id, name = "num_redeemed"), 
  by = "household_id"
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))

#Thus 26.3% percent of households that received the retailer’s weekly mailer redeemed at least one coupon


#Question 2: How many households received and did not redeem a coupon?
left_join(
  campaigns          %>% count(household_id, name = "num_received"),
  coupon_redemptions %>% count(household_id, name = "num_redeemed"), 
  by = "household_id"
) %>% 
  summarize(num_household_didnt_redeem = sum(is.na(n.y)))
# 1149 households received and did not redeem a coupon


#Question 3: What percentage of coupons promoted in the retailer’s weekly mailer got redeemed at least once?
left_join(
  coupons %>% count(coupon_upc, name = "num_coupon_sent"),
  coupon_redemptions %>% count(coupon_upc, name = "num_redeemed"), 
  by = "coupon_upc"
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))
#  redemption_rate
#    0.501
#Thus, 50.1% coupons promoted in the retailer’s weekly mailer got redeemed at least once


#Question 4: Considering the product categories that the 801 households in the Complete Journey Study purchased most heavily, which five categories did they start spending more on at the highest rate over the course of Q1? Only consider product categories that the group spent $1,500 or more on in January, and calculate spend growth as a percentage of category spend in January.
left_join(transactions,products,by= "product_id") %>% 
  mutate(month = month(transaction_timestamp,label=T)) %>% 
  group_by(product_category,month) %>% 
  summarise(total_spending = sum(sales_value)) %>% 
  filter(month %in% c("Jan","Mar")) %>% 
  mutate(spend_growth_pct = (lead(total_spending)-total_spending)/total_spending*100) %>% 
  filter(month=="Jan", total_spending >=1500) %>% 
  arrange(desc(spend_growth_pct)) %>% 
  head(5)

#  product_category      month total_spending spend_growth_pct
#1 INFANT FORMULA        Jan            1948.             53.4
#2 SEAFOOD - FROZEN      Jan            3038.             29.8
#3 CANDY - PACKAGED      Jan            2582.             23.3
#4 ORAL HYGIENE PRODUCTS Jan            1561.             19.5
#5 DOMESTIC WINE         Jan            2371.             13.5
#Thus, INFANT FORMULA, SEAFOOD - FROZEN, CANDY - PACKAGED, ORAL HYGIENE PRODUCTS and DOMESTIC WINE people start spending more on at the highest rate over the course of Q1