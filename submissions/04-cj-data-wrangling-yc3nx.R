library(tidyverse)
library(tidyr)
library(completejourney)
library(lubridate)

# Question 1: What percent of households that received the retailer’s weekly mailer redeemed at least one coupon?
# 26.3%
campaigns %>% 
  count(household_id, name = "n_recipients")%>%
  left_join(coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = "household_id"
) %>% 
  summarize(redemption_rate = mean(!is.na(n_redemptions)))

  
  # Question 2: How many households received and did not redeem a coupon?
# 1149
campaigns %>% 
  count(household_id, name = "n_recipients")%>%
  left_join(coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
            by = "household_id"
  ) %>% 
  summarize(redemption_rate = sum(is.na(n_redemptions)))


  # Question 3: What percentage of coupons promoted in the retailer’s weekly mailer got redeemed at least once?
coupons %>%
  left_join(coupon_redemptions, by = c("coupon_upc"))%>%
  summarize(pct_redeemed = mean(!is.na(redemption_date)))

coupons%>% 
  count(coupon_upc, name = "n_products", sort = TRUE)%>%
left_join(coupon_redemptions %>% count(coupon_upc, name = "n_redemptions"), 
  by = "coupon_upc") %>% 
  summarize(redemption_rate = mean(!is.na(n_redemptions)))

# 50.1% was redeemed. 
  
  # Question 4: Considering the product categories that the 801 households in the Complete Journey Study purchased most heavily, which five categories did they start spending more on at the highest rate over the course of Q1? Only consider product categories that the group spent $1,500 or more on in January, and calculate spend growth as a percentage of category spend in January.

transactions %>%
  mutate(month = month(transaction_timestamp)) %>%
  filter(month %in% c(1,3))%>%
  left_join(products)%>%
  group_by(product_category,month)%>%
  summarize(total_spending = sum(sales_value))%>%
  mutate(spend_growth_pct = (total_spending - lag(total_spending, order_by = month))/lag(total_spending, order_by = month), 
         jan_spend = first(total_spending))%>%
  filter(jan_spend >= 1500) %>%
  arrange(desc(spend_growth_pct))%>%
  select(product_category,spend_growth_pct)
  
# the spending on infant formula grew by 53.4% since january. So did candy, sigfifies a growth the underaged population in the customer base. Other top 5s are frozen seafood, oral hygiene product and domestic wine. 


  
  
  
  
  