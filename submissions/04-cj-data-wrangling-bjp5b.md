Answer to the Data Wrangling Excercises
================
Brian J. Policard
April 2nd, 2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0       ✔ purrr   0.3.2  
    ## ✔ tibble  2.1.1       ✔ dplyr   0.8.0.1
    ## ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
    ## ✔ readr   1.3.1       ✔ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(completejourney)
library(lubridate) 
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

**Question 1**: What percent of households that received the retailer's weekly mailer redeemed at least one coupon?

``` r
left_join(
  campaigns          %>% count(household_id, name = "n_recipients"),
  coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = "household_id"
) %>% 
  summarize(redemption_rate = mean(!is.na(n_redemptions)))
```

    ## # A tibble: 1 x 1
    ##   redemption_rate
    ##             <dbl>
    ## 1           0.263

26.3% redeemed at least one coupon. **Question 2**: How many households received and did not redeem a coupon?

``` r
left_join(
  campaigns          %>% count(household_id, name = "n_recipients"),
  coupon_redemptions %>% count(household_id, name = "n_redemptions"), 
  by = "household_id"
) %>% 
  summarize(redemption_rate = sum(is.na(n_redemptions))) 
```

    ## # A tibble: 1 x 1
    ##   redemption_rate
    ##             <int>
    ## 1            1149

1,149 households didn't reedem a coupon **Question 3**: What percentage of coupons promoted in the retailer's weekly mailer got redeemed at least once?

``` r
left_join(
  coupons            %>% count(coupon_upc, name = "n_products", sort = TRUE),
  coupon_redemptions %>% count(coupon_upc, name = "n_redemptions"), 
  by = "coupon_upc") %>% 
  summarize(redemption_rate = sum(!is.na(n_redemptions)))
```

    ## # A tibble: 1 x 1
    ##   redemption_rate
    ##             <int>
    ## 1             491

491 out of 981 (about 50%) redeemed at least once **Question 4**: Considering the product categories that the 801 households in the Complete Journey Study purchased most heavily, which five categories did they start spending more on at the highest rate over the course of Q1? Only consider product categories that the group spent $1,500 or more on in January, and calculate spend growth as a percentage of category spend in January.

``` r
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
```

    ## # A tibble: 5 x 2
    ## # Groups:   product_category [5]
    ##   product_category      spend_growth_pct
    ##   <chr>                            <dbl>
    ## 1 INFANT FORMULA                    53.4
    ## 2 SEAFOOD - FROZEN                  29.8
    ## 3 CANDY - PACKAGED                  23.3
    ## 4 ORAL HYGIENE PRODUCTS             19.5
    ## 5 DOMESTIC WINE                     13.5

Instant Formula (grew 53.4%), Seafood - Frozen (grew 29.8%), Candy - packaged (grew 23.3%), Oral Hygiene products (grew 19.5%), Domsestic Wine (grew 13.5%)
