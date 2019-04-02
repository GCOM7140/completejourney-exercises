library(tidyverse)
library(lubridate)
library(completejourney)

#' Question 1: What percent of households that received the retailer’s weekly
#' mailer redeemed at least one coupon?

coupon_redemptions

campaigns

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




