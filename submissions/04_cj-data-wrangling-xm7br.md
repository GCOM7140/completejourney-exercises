#' ---
#' title: "Answers to the Data Wrangling Exercises"
#' author: Amanda Ma
#' date: 03/31/2019
#' output: github_document
#' ---


```{r}
library(tidyverse)
library(completejourney)
library(lubridate) # see chapter 16 of r4ds
```

---
  
**Question 1**: What percent of households that received the retailer's weekly mailer redeemed at least one coupon?


```{r}
left_join(
  campaigns %>% 
    count(household_id, name = "receipients"),
  coupon_redemptions %>% 
    count(household_id, name = "redemptions"), 
  by = "household_id"
          ) %>% 
summarize(redemption_rate = mean(!is.na(redemptions)))
# 1,559 households received mailed coupons as part of a campaign, 26.3% redeemed a coupon.
```

---
**Question 2**: How many households received and did not redeem a coupon?   

```{r}
  left_join(
    campaigns %>% 
      count(household_id, name = "receipients"),
    coupon_redemptions %>% 
      count(household_id, name = "redemptions"), 
    by = "household_id"
  ) %>% 
  summarize(redemption_rate = sum(is.na(redemptions)))
# 1,149 households received and did not redeem a coupon.
```
---
**Question 3**: What percentage of coupons promoted in the retailer's weekly mailer got redeemed at least once?

```{r}

left_join(
  coupons %>% 
    count(coupon_upc, name = "coupons_promoted", sort = TRUE),
  coupon_redemptions %>% 
    count(coupon_upc, name = "redemptions"), 
  by = "coupon_upc"
) %>% 
  summarize(redemption_rate = sum(!is.na(redemptions)))

# Of the 981 coupons promoted in the retailer's weekly mailer, 491 (or 50%) of them were redeemed at least once.
```

---
**Question 4**: Considering the product categories that the 801 households in the Complete Journey Study purchased most heavily, which five categories did they start spending more on at the highest rate over the course of Q1? Only consider product categories that the group spent $1,500 or more on in January, and calculate spend growth as a percentage of category spend in January.

Here are some suggestions:  
1. Join the `transactions` and `products` datasets.  
2. Get month from `transaction_timestamp` and name this variable `month`.  
3. Group by `product_category` and `month`.  
4. Calculate total spend at the category-month level using `sales_value`.  
5. Filter the data to include only the months of January and March.  
6. Create a new variable called `spend_growth_pct`, making use of the [`lag()`][5.5.1] function.  
7. Filter for categories that had category spend of \$1,500 or more in January. 
8. Arrange the data in descending order according to `spend_growth_pct`.  

```{r}
transactions %>%
  left_join(products, by = "product_id") %>%
  mutate(month = month(transaction_timestamp)) %>%
  group_by(product_category, month) %>%
  summarize(sales_value = sum(sales_value)) %>%
  filter(month %in% c("1","3")) %>%
  mutate(
    spend_growth_pct = (sales_value - lag(sales_value)) / lag(sales_value) *100,
    spend_jan = first(sales_value)) %>%
  filter(spend_jan >=1500) %>%
  arrange(desc(spend_growth_pct))

The top five product categories are infant formula (grew 53.4%), frozen seafood (29.8%), packaged candy (23.3%), oral hygiene products (19.5%), and domestic wine (13.5%. 

```