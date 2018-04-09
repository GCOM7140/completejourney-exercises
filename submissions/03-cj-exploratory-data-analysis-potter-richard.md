Exploratory Data Analysis (EDA) Exercises
================
Richard Potter
4/8/2018

------------------------------------------------------------------------

**Initial loadings and joins**

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(completejourney)

left_join(transaction_data, product) %>% 
  left_join(hh_demographic) %>% 
  filter(
    quantity != 0
  ) %>% 
  mutate(
    regular_price  = (sales_value + retail_disc + coupon_match_disc) /
      quantity,
    loyalty_price  = (sales_value + coupon_match_disc) / 
      quantity,
    coupon_price   = (sales_value - coupon_disc) / 
      quantity,
    purchase_price = ifelse(coupon_disc > 0, coupon_price, 
                            ifelse(retail_disc > 0, loyalty_price,
                                   regular_price))
  ) -> my_transaction_data
```

    ## Joining, by = "product_id"

    ## Joining, by = "household_key"

------------------------------------------------------------------------

**Exercise 1**

**Question**: How many unique households exist in `my_transaction_data`, and how many of these households in `my_transaction_data` have demographic data in `hh_demographic`?

**Answer**: 2,500 households in the `my_transaction_data` dataset and 801 households also include demographic data.

``` r
my_transaction_data %>% 
  distinct(household_key) %>% 
  nrow()
```

    ## [1] 2500

``` r
inner_join(my_transaction_data, hh_demographic) %>% 
  distinct(household_key) %>% 
  nrow()
```

    ## Joining, by = c("household_key", "age_desc", "marital_status_code", "income_desc", "homeowner_desc", "hh_comp_desc", "household_size_desc", "kid_category_desc")

    ## [1] 801

------------------------------------------------------------------------

**Exercise 2**

**Question**: Determine median weekly spend per individual using the following tibble (i.e., `exercise_2`).

**Answer**: 19.4

``` r
inner_join(my_transaction_data, hh_demographic) %>% 
  mutate(
    hh_size          = str_replace(household_size_desc, '5\\+', '5') %>% 
      as.integer()
  ) %>% 
  group_by(household_key, week_no) %>% 
  summarize(
    total_spend      = sum(purchase_price, na.rm = TRUE),
    hh_size          = max(hh_size,        na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  mutate(
    wkly_spend_per_ind = total_spend / hh_size
  ) -> exercise_2
```

    ## Joining, by = c("household_key", "age_desc", "marital_status_code", "income_desc", "homeowner_desc", "hh_comp_desc", "household_size_desc", "kid_category_desc")

``` r
exercise_2 %>% 
  summarise(median_week = median(wkly_spend_per_ind, na.rm = T))
```

    ## # A tibble: 1 x 1
    ##   median_week
    ##         <dbl>
    ## 1        19.4

------------------------------------------------------------------------

**Exercise 3**

**Question**: Building on Exercise 2, plot median spend per individual for the five household sizes in `my_transaction_data`.

**Answer**: See below.

``` r
exercise_2 %>% 
  group_by(hh_size) %>% 
  summarise(median_week = median(wkly_spend_per_ind, na.rm = T)) %>% 
  ggplot(aes(x = hh_size, y = median_week)) +
  geom_col()
```

![](03-cj-exploratory-data-analysis-potter-richard_files/figure-markdown_github/unnamed-chunk-4-1.png)

------------------------------------------------------------------------

**Exercise 4**

**Question**: Are baskets with diapers in them more likely than average to have beer in them too? Legend has it that placing these two product categories closer together can increase beer sales (Powers 2002). Using the following starter code, calculate lift for the "association rule" that diapers in a basket (i.e., `sub_commodity_desc == 'BABY DIAPERS'`) imply beer is in the basket (i.e., `sub_commodity_desc == 'BEERALEMALT LIQUORS'`). Is the association between these products practically significant in `my_transaction_data`?

**Answer**: Yes, there is a significant lift.

``` r
inner_join(my_transaction_data, product) %>% 
  mutate(
    diapers = sub_commodity_desc == 'BABY DIAPERS', 
    beer    = sub_commodity_desc == 'BEERALEMALT LIQUORS'
  ) %>% 
  group_by(basket_id) %>% 
  summarise(beer_basket = max(beer), diaper_basket = max(diapers)) %>% 
  summarise(
    prop_both   = sum(beer_basket * diaper_basket == 1) / 
                  sum(diaper_basket == 1),
    prob_beer   = mean(beer_basket),
    diaper_lift = prop_both / prob_beer
  )
```

    ## Joining, by = c("product_id", "manufacturer", "department", "brand", "commodity_desc", "sub_commodity_desc", "curr_size_of_product")

    ## # A tibble: 1 x 3
    ##   prop_both prob_beer diaper_lift
    ##       <dbl>     <dbl>       <dbl>
    ## 1    0.0602    0.0566        1.06

------------------------------------------------------------------------

**Exercise 5**

**Question**: Using a stacked bar chart that's partitioned by income level (i.e., `income_desc`), visualize the total amount of money customers spent on national-brand products versus private-label products.

**Answer**: See below.

``` r
inner_join(my_transaction_data, hh_demographic) %>% 
  mutate(income_desc = factor(income_desc, 
                            levels = c('Under 15K',   '15-24K',   '25-34K', 
                                       '35-49K',   '50-74K',   '75-99K', 
                                       '100-124K', '125-149K', '150-174K', 
                                       '175-199K', '200-249K',    '250K+'),
                                       ordered = TRUE)) %>%
  group_by(income_desc, brand) %>%
  summarize(total_spend = sum(purchase_price)) %>% 
  ggplot() +
  geom_col(aes(x = income_desc, y = total_spend, fill = brand), 
  position = 'fill') + 
  theme(axis.text.x = element_text(angle = 45))
```

    ## Joining, by = c("household_key", "age_desc", "marital_status_code", "income_desc", "homeowner_desc", "hh_comp_desc", "household_size_desc", "kid_category_desc")

![](03-cj-exploratory-data-analysis-potter-richard_files/figure-markdown_github/unnamed-chunk-6-1.png)
