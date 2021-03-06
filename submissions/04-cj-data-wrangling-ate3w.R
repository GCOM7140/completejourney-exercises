# Data Wrangling

# Question 1
left_join(
  campaigns          %>% count(household_id, name = "n_recipients"),
  coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = 'household_id'
) %>% 
  summarize(redemption_rate = mean(!is.na(n_redemptions)))
#.263

# Question 2
left_join(
  campaigns          %>% count(household_id, name = "n_recipients"),
  coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = 'household_id'
) %>% 
  summarize(redemption_rate = sum(is.na(n_redemptions)))
#1149

# Question 3
left_join(
  coupons            %>% count(coupon_upc, name = "n_products", sort = TRUE),
  coupon_redemptions %>% count(coupon_upc, name = "n_redemptions"), 
  by = "coupon_upc"
) %>% 
  summarize(redemption_rate = sum(!is.na(n_redemptions)))
#491

# Question 4
left_join(transactions, products, by = "product_id") %>%
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
##   product_category      spend_growth_pct
##   <chr>                            <dbl>
## 1 INFANT FORMULA                    53.4
## 2 SEAFOOD - FROZEN                  29.8
## 3 CANDY - PACKAGED                  23.3
## 4 ORAL HYGIENE PRODUCTS             19.5
## 5 DOMESTIC WINE                     13.5