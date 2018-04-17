Data Wrangling Exercises
================
Richard Potter
04/17/18

``` r
library(tidyverse)
library(completejourney)
```

------------------------------------------------------------------------

Exercise 1:
-----------

**Question**: What percent of households that received the retailer's weekly mailer redeemed at least one coupon?

**Answer**:

``` r
left_join(campaign_table %>% count(household_key),
          coupon_redempt %>% count(household_key),
          by = "household_key")%>% 
  summarise(redrate = (mean(!is.na(n.y))))
```

    ## # A tibble: 1 x 1
    ##   redrate
    ##     <dbl>
    ## 1   0.274

------------------------------------------------------------------------

Exercise 2:
-----------

**Question**: How many households did not redeem a coupon?

**Answer**:

``` r
left_join(campaign_table %>% count(household_key),
          coupon_redempt %>% count(household_key),
          by = "household_key") %>% 
  summarise(redrate = (sum(is.na(n.y))))
```

    ## # A tibble: 1 x 1
    ##   redrate
    ##     <int>
    ## 1    1150

------------------------------------------------------------------------

Exercise 3:
-----------

**Question**: What percent of coupons promoted in the retailer's weekly mailer got redeemed at least once?

**Answer**:

``` r
left_join(
  coupon %>% 
  count(coupon_upc), coupon_redempt %>%
  count(coupon_upc), by = "coupon_upc") %>% 
  summarise(redrate = mean(!is.na(n.y)))
```

    ## # A tibble: 1 x 1
    ##   redrate
    ##     <dbl>
    ## 1   0.490

------------------------------------------------------------------------

Exercise 4:
-----------

**Question**: Using the `transaction_data` and `product` datasets, determine which product category (i.e., `sub_commodity_desc`) grew the most in terms of revenue for the retailer in the second half of the study period (i.e., between `week_no == 52` and `week_no == 102`). Only consider product categories that had over $100 in revenue in week 52, and calculate revenue growth as a percentage of category revenue in week 52.

**Answer**:

``` r
left_join(transaction_data, product) %>% 
  group_by(sub_commodity_desc, week_no) %>% 
  summarise(totalsales = sum(sales_value)) %>% 
  filter(week_no == 52 | week_no == 102) %>% 
  mutate(revenue_growth = ifelse(lag(totalsales) >= 100,
                                 (totalsales - lag(totalsales)) /
                                  lag(totalsales), NA)) %>% 
  arrange(desc(revenue_growth))
```

    ## Joining, by = "product_id"

    ## # A tibble: 2,691 x 4
    ## # Groups:   sub_commodity_desc [1,520]
    ##    sub_commodity_desc             week_no totalsales revenue_growth
    ##    <chr>                            <int>      <dbl>          <dbl>
    ##  1 SELECT BEEF                        102       356.          0.852
    ##  2 LUNCH COMBO                        102       240.          0.827
    ##  3 CHICKEN BREAST BONE IN             102       243.          0.495
    ##  4 ENHANCED                           102       723.          0.403
    ##  5 RAZORS AND BLADES                  102       202.          0.387
    ##  6 ADULT CEREAL                       102       324.          0.381
    ##  7 ADULT ANALGESICS                   102       258.          0.378
    ##  8 PIZZA/ECONOMY                      102       139.          0.372
    ##  9 NON-CRBNTD DRNKING/MNERAL WATE     102       258.          0.363
    ## 10 SCOOP LITTER                       102       166.          0.305
    ## # ... with 2,681 more rows
