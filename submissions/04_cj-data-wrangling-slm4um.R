library(tidyverse)
library(lubridate)
library(completejourney)

#' Question 1: What percent of households that received the retailer’s weekly
#' mailer redeemed at least one coupon?

coupon_redemptions

coupons

left_join(
 campaigns %>% count(household_id, name = "wm"),
 coupon_redemptions %>% count(household_id, name = "cr"),
 by = "household_id"
) -> newtab

redeemed <- sum(!is.na(newtab$cr))
week_mailer <- sum(!is.na(newtab$wm))

Percenthouse <- (redeemed/week_mailer) * 100
Percenthouse

#' Question 2: How many households received and did not redeem a coupon?

Noredeem <- week_mailer - redeemed
Noredeem

#' Question 3: What percentage of coupons promoted in the retailer’s weekly
#' mailer got redeemed at least once?

left_join(
  coupons %>% count(coupon_upc, name = "mailed"),
  coupon_redemptions %>% count(coupon_upc, name = "redeem")
) %>%
  summarize(rc = mean(!is.na(redeem)))

#about 50%

#' Question 4: Considering the product categories that the 801 households in the
#' Complete Journey Study purchased most heavily, which five categories did they
#' start spending more on at the highest rate over the course of Q1? Only
#' consider product categories that the group spent $1,500 or more on in
#' January, and calculate spend growth as a percentage of category spend in
#' January.

transactions %>% 
  left_join(products, by = "product_id") %>%
  mutate(month = month(transaction_timestamp, label = TRUE)) %>%
  group_by(product_category, month) %>%
  summarize(spend_tot = sum(sales_value, na.rm = TRUE)) %>%
  filter(month == "Jan" | month == "Feb" | month == "Mar") %>%
  group_by(product_category) %>% 
  mutate(
    spend_growth_pct = ((spend_tot - lag(spend_tot)) / lag(spend_tot)) * 100,
    spend_jan = first(spend_tot)
  ) %>% 
  filter(spend_jan >= 1500) %>% 
  arrange(desc(spend_growth_pct)) %>% 
  select(product_category, spend_growth_pct) %>% 
  head(5)

