Data Wrangling Exercises
================

The following four exercises are based on concepts covered in [Chapter 12](http://r4ds.had.co.nz/tidy-data.html) and [Chapter 13](http://r4ds.had.co.nz/relational-data.html) of [R for Data Science](http://r4ds.had.co.nz/). Use the `coupon`, `coupon_redempt`, `campaign_table`, `transaction_data`, and `product` datasets that come with the `completejourney` package to work on them, and start by loading the `tidyverse` and `completejourney` packages.

``` r
library(tidyverse)
library(completejourney)
```

------------------------------------------------------------------------

Exercise 1
----------

What percent of households that received the retailer's weekly mailer redeemed at least one coupon?

### Solution to Exercise 1

Of the 1,584 households that were mailed coupons as part of a campaign, 434 (or 27.4%) redeemed a coupon.

``` r
left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))
```

    ## # A tibble: 1 x 1
    ##   redemption_rate
    ##             <dbl>
    ## 1           0.274

------------------------------------------------------------------------

Exercise 2
----------

How many households did not redeem a coupon?

### Solution to Exercise 2

1,150 households did not redeem a coupon.

``` r
left_join(
  campaign_table %>% count(household_key),
  coupon_redempt %>% count(household_key), by = 'household_key'
) %>% 
  summarize(redemption_rate = sum(is.na(n.y)))
```

    ## # A tibble: 1 x 1
    ##   redemption_rate
    ##             <int>
    ## 1            1150

------------------------------------------------------------------------

Exercise 3
----------

What percent of coupons promoted in the retailer's weekly mailer got redeemed at least once?

### Solution to Exercise 3

Of the 1,135 coupons promoted in the retailer's weekly mailer, 556 (or 49%) coupons were redeemed at least once.

``` r
left_join(
  coupon %>% count(coupon_upc),
  coupon_redempt %>% count(coupon_upc), by = 'coupon_upc'
) %>% 
  summarize(redemption_rate = mean(!is.na(n.y)))
```

    ## # A tibble: 1 x 1
    ##   redemption_rate
    ##             <dbl>
    ## 1           0.490

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

### Solution to Exercise 4

Having grown 85.2% in the second half of the study period, the **select beef** category grew the most among all product categories.

``` r
left_join(transaction_data, product) %>%
  group_by(sub_commodity_desc, week_no) %>%
  summarize(sales_value_category = sum(sales_value, na.rm = TRUE)) %>%
  filter(week_no == 52 | week_no == 102) %>%
  mutate(
    revenue_growth = ifelse(lag(sales_value_category) >= 100,
                            (sales_value_category - lag(sales_value_category)) /
                             lag(sales_value_category), NA)
  ) %>%
  arrange(desc(revenue_growth))
```

    ## # A tibble: 2,691 x 4
    ## # Groups:   sub_commodity_desc [1,520]
    ##    sub_commodity_desc             week_no sales_value_cate… revenue_growth
    ##    <chr>                            <int>             <dbl>          <dbl>
    ##  1 SELECT BEEF                        102              356.          0.852
    ##  2 LUNCH COMBO                        102              240.          0.827
    ##  3 CHICKEN BREAST BONE IN             102              243.          0.495
    ##  4 ENHANCED                           102              723.          0.403
    ##  5 RAZORS AND BLADES                  102              202.          0.387
    ##  6 ADULT CEREAL                       102              324.          0.381
    ##  7 ADULT ANALGESICS                   102              258.          0.378
    ##  8 PIZZA/ECONOMY                      102              139.          0.372
    ##  9 NON-CRBNTD DRNKING/MNERAL WATE     102              258.          0.363
    ## 10 SCOOP LITTER                       102              166.          0.305
    ## # ... with 2,681 more rows
