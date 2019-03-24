Exploratory Data Analysis (EDA) Exercises
================

The following questions are based on concepts covered in [Chapter 7](http://r4ds.had.co.nz/exploratory-data-analysis.html) of R4DS, and answers to them lie in the `transactions` and `products` datasets of the completejourney package. Load the tidyverse, completejourney, and lubridate packages to start working on them.

``` r
library(tidyverse)
library(completejourney)
```

After running the following code, use `my_transaction_data` to work on the following exercises.

``` r
left_join(transactions, products, by = "product_id") %>% 
   left_join(demographics, by = "household_id") %>%
   filter(quantity != 0) %>%
   mutate(
     regular_price  = (sales_value + retail_disc + coupon_match_disc) /
                       quantity,
     loyalty_price  = (sales_value + coupon_match_disc) / 
                       quantity,
     coupon_price   = (sales_value - coupon_disc) / 
                       quantity,
     purchase_price = ifelse(coupon_disc > 0, coupon_price, 
                             ifelse(retail_disc > 0, loyalty_price,
                                                             regular_price
       )
     )
   ) -> my_transaction_data
```

------------------------------------------------------------------------

**Question 1**: How many unique households exist in `my_transaction_data`, and how many of these households have demographic data in `demographics`?

1.  Use `distinct()` to create a tibble of unique `household_id` values.
2.  Use `nrow()` to count these households.
3.  Use `inner_join()` to match `my_transaction_data` with `demographics`.
4.  Use `distinct()` and `nrow()` to count the rows that remain.

------------------------------------------------------------------------

**Question 2**: Determine median weekly spend per individual using the following tibble (i.e., `exercise_2`).

``` r
inner_join(my_transaction_data, demographics) %>% 
   mutate(
     hh_size          = str_replace(household_size, "5\\+", "5") %>% 
                         as.integer()
   ) %>% 
   group_by(household_id, week) %>% 
   summarize(
     total_spend      = sum(purchase_price, na.rm = TRUE),
     hh_size          = max(hh_size,        na.rm = TRUE)
   ) %>% 
   ungroup() %>%
  mutate(
    wkly_spend_per_ind = total_spend / hh_size
  ) -> exercise_2
```

------------------------------------------------------------------------

**Question 3**: Building on Question 2, plot median spend per individual for the five household sizes in `my_transaction_data`.

------------------------------------------------------------------------

**Question 4**: Are baskets with diapers in them more likely than average to have beer in them too? Legend has it that placing these two product categories closer together can increase beer sales [(Powers 2002)](https://www.theregister.co.uk/2006/08/15/beer_diapers/). Using the following starter code, calculate [lift](https://en.wikipedia.org/wiki/Lift_(data_mining)) for the "association rule" that diapers in a basket (i.e., `product_type` == "BABY DIAPERS"`) imply beer is in the basket (i.e.,`product\_type`== "BEERALEMALT LIQUORS"`). Is the association between these products practically significant in `my_transaction_data`?

``` r
inner_join(my_transaction_data, products) %>% 
  mutate(
    diapers = product_type == "BABY DIAPERS", 
    beer    = product_type == "BEERALEMALT LIQUORS"
  )
```

------------------------------------------------------------------------

**Question 5**: Using a stacked bar chart that's partitioned by income level (i.e., `income`), visualize the total amount of money customers spent on national-brand products versus private-label products. Start with the following code:

``` r
inner_join(my_transaction_data, demographics) %>% 
  mutate(
    income = factor(income, 
                         levels = c("Under 15K",   "15-24K",   "25-34K", 
                                       "35-49K",   "50-74K",   "75-99K", 
                                     "100-124K", "125-149K", "150-174K", 
                                     "175-199K", "200-249K",    "250K+"),
                         ordered = TRUE)
  )
```
