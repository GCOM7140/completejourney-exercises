Data Wrangling Exercises
================
Zoe Zheng
April 17, 2018

The following four exercises are based on concepts covered in [Chapter 12](http://r4ds.had.co.nz/tidy-data.html) and [Chapter 13](http://r4ds.had.co.nz/relational-data.html) of [R for Data Science](http://r4ds.had.co.nz/). Use the `coupon`, `coupon_redempt`, `campaign_table`, `transaction_data`, and `product` datasets that come with the `completejourney` package to work on them, and start by loading the `tidyverse` and `completejourney` packages.

``` r
library(tidyverse)
library(completejourney)
```

------------------------------------------------------------------------

Exercise 1
----------

What percent of households that received the retailer's weekly mailer redeemed at least one coupon?

``` r
redeem_coupon <- left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
)

redemption_rate <- mean(!is.na(redeem_coupon$n.y)) 

redemption_rate
```

    ## [1] 0.2739899

The coupon redemption rate is 0.27.

------------------------------------------------------------------------

Exercise 2
----------

How many households did not redeem a coupon?

``` r
not_redeem <- sum(is.na(redeem_coupon$n.y))

not_redeem 
```

    ## [1] 1150

1150 household did not redeem a coupon.

------------------------------------------------------------------------

Exercise 3
----------

What percent of coupons promoted in the retailer's weekly mailer got redeemed at least once?

``` r
coupon_count <- left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by = 'coupon_upc'
) 

coupon_rate <- mean(!is.na(coupon_count$n.y))
coupon_rate
```

    ## [1] 0.4898678

49% of the coupons got redeemed at least once.

------------------------------------------------------------------------

Exercise 4
----------

Using the `transaction_data` and `product` datasets, determine which product category (i.e., `sub_commodity_desc`) grew the most in terms of revenue for the retailer in the second half of the study period (i.e., between `week_no == 52` and `week_no == 102`). Only consider product categories that had over $100 in revenue in week 52, and calculate revenue growth as a percentage of category revenue in week 52.

Here are some suggested steps:

1.  Join the `transaction_data` and `product` datasets.
2.  Group the data by `sub_commodity_desc` and `week_no`.
3.  Calculate `sales_value` at the category level.
4.  Filter the data to only include category revenues in weeks 52 and 102.
5.  Create a new variable called `revenue_growth`, making use of the `lag()` function. Recognize that you only want to consider categories that had category revenues of at least $100 in week 52.
6.  Arrange the data in descending order according to `revenue_growth`.

``` r
revgrowth <- left_join(transaction_data, product, by="product_id") %>% 
  group_by(sub_commodity_desc, week_no) %>% 
  summarise(total_rev = sum(sales_value, na.rm = TRUE)) %>% 
  filter(week_no == 52 | week_no == 102) %>% 
  mutate(
    rev_growth = ifelse(lag(total_rev) >= 100, 
    (total_rev - lag(total_rev))/lag(total_rev), NA)
    )
  
revgrowth <- arrange(revgrowth, desc(rev_growth))

head(revgrowth)
```

    ## # A tibble: 6 x 4
    ## # Groups:   sub_commodity_desc [6]
    ##   sub_commodity_desc     week_no total_rev rev_growth
    ##   <chr>                    <int>     <dbl>      <dbl>
    ## 1 SELECT BEEF                102      356.      0.852
    ## 2 LUNCH COMBO                102      240.      0.827
    ## 3 CHICKEN BREAST BONE IN     102      243.      0.495
    ## 4 ENHANCED                   102      723.      0.403
    ## 5 RAZORS AND BLADES          102      202.      0.387
    ## 6 ADULT CEREAL               102      324.      0.381
