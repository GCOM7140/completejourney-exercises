#############################
# Chris Dowdy April 2       #
# completejourney-exercises #
# Data Wrangling            #
#############################

library(tidyverse)
library(completejourney)
library(lubridate)


# Question 1: What percent of households that received the retailer’s weekly
# mailer redeemed at least one coupon?

left_join(
  campaigns          %>% count(household_id, name = "n_recipients"),
  coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = "household_id"
) %>% 
  summarize(redemption_rate = mean(!is.na(n_redemptions)))

# Of the 1,559 households that were mailed coupons as part of a campaign, 410
# (or 26.3%) redeemed a coupon.


# Question 2: How many households received and did not redeem a coupon?

left_join(
  campaigns          %>% count(household_id, name = "n_recipients"),
  coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = "household_id"
) %>% 
  summarize(redemption_rate = sum(is.na(n_redemptions)))

# 1,149 households received and did not redeem a coupon.


# Question 3: What percentage of coupons promoted in the retailer’s weekly
# mailer got redeemed at least once?

left_join(
  coupons            %>% count(coupon_upc, name = "n_products", sort = TRUE),
  coupon_redemptions %>% count(coupon_upc, name = "n_redemptions"), 
  by = "coupon_upc") %>% 
  summarize(redemption_rate = sum(!is.na(n_redemptions)))

# Of the 981 coupons promoted in the retailer's weekly mailer, 491 (or 50%) of
# them were redeemed at least once.


# Question 4: Considering the product categories that the 801 households in the
# Complete Journey Study purchased most heavily, which five categories did they
# start spending more on at the highest rate over the course of Q1? Only
# consider product categories that the group spent $1,500 or more on in January,
# and calculate spend growth as a percentage of category spend in January.

transactions %>% 
  left_join(products, by = "product_id") %>%
  mutate(month = month(transaction_timestamp, label = TRUE)) %>%
  group_by(product_category, month) %>%
  summarize(spend_tot = sum(sales_value, na.rm = TRUE)) %>%
  filter(month == "Jan" | month == "Mar") %>%
  group_by(product_category) %>% 
  mutate(
    spend_growth_pct = (spend_tot - lag(spend_tot)) / lag(spend_tot) * 100,
    spend_jan = first(spend_tot)
  ) %>% 
  filter(spend_jan >= 1500) %>% 
  arrange(desc(spend_growth_pct)) %>% 
  select(product_category, spend_growth_pct) %>% 
  head(5)

# From January to March, the 2,469 households in the Complete Journey Study
# increased their spend on infant formula by 53.4 percent. They spent almost 30
# percent more on frozen seafood, just over 23 percent more on packaged candy,
# nearly 20 percent more on oral hygiene products, and 13.5 percent more on
# domestic wine.