#' ------
#' title: "Answer to the data wrangling assignment"
#' author: YimingWu
#' date: 04/01/2019
#' output: github_document
#' ------
#' 
#' 
library(tidyverse)
library(completejourney)
library(lubridate)

#' **Q1**: What percent of households that received the retailer’s weekly mailer redeemed at least one coupon?

left_join(
  campaigns          %>% count(household_id, name = "n_recipients"),
  coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = "household_id"
) %>% 
  summarize(redemption_rate = mean(!is.na(n_redemptions)))

# 26.3% households that received the retailer’s weekly mailer redeemed at least one coupon.


#' **Q2**: How many households received and did not redeem a coupon?

left_join(
  campaigns          %>% count(household_id, name = "n_recipients"),
  coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = "household_id"
) %>% 
  summarize(redemption_rate = sum(is.na(n_redemptions)))

# 1149  households received and did not redeem a coupon.

#' **Q3**: What percentage of coupons promoted in the retailer’s weekly mailer got redeemed at least once?

left_join(
  coupons            %>% count(coupon_upc, name = "n_products", sort = TRUE),
  coupon_redemptions %>% count(coupon_upc, name = "n_redemptions"), 
  by = "coupon_upc") %>% 
  summarize(redemption_rate = sum(!is.na(n_redemptions)))

491/981

# 491 coupons promoted in the retailer’s weekly mailer got redeemed at least once, it's about 50% (981 in total).

#' **Q4**: Considering the product categories that the 801 households in the Complete Journey Study purchased most heavily, which five categories did they start spending more on at the highest rate over the course of Q1? Only consider product categories that the group spent $1,500 or more on in January, and calculate spend growth as a percentage of category spend in January.

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
  head(6)

# increased spent on infant formula by 53.4% 
# spent 29.8% on frozen sea food
# spent about 23% on packaged candy
# about 20% on oral hygiene products and 13.5% on domestic wine. 
# No.6 berries 12.2%
