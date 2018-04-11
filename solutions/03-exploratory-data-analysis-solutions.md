Exploratory Data Analysis (EDA) Solutions
================

The following seven exercises are based on concepts covered in [Chapter 7 of R4DS](http://r4ds.had.co.nz/exploratory-data-analysis.html). They can be answered using datasets from the `completejourney` package. Start by loading the `tidyverse` and `completejourney` packages.

``` r
library(tidyverse)
library(completejourney)
```

After running the following code, use `my_transaction_data` to work on the following exercises.

``` r
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

------------------------------------------------------------------------

Exercise 1
----------

How many unique households exist in `my_transaction_data`, and how many of these households in `my_transaction_data` have demographic data in `hh_demographic`?

1.  Use `distinct()` to create a tibble of unique `household_key` values.
2.  Use `nrow()` to count these households.
3.  Use `inner_join()` to match `my_transaction_data` with `hh_demographic`.
4.  Use `distinct()` and `nrow()` to count the rows that remain.

**Answer:**

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

    ## [1] 801

------------------------------------------------------------------------

Exercise 2
----------

Determine median weekly spend per individual using the following tibble (i.e., `exercise_2`).

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

**Answer:**

``` r
exercise_2 %>% 
  summarize(
    med_wkly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)
  )
```

    ## # A tibble: 1 x 1
    ##   med_wkly_spend_per_ind
    ##                    <dbl>
    ## 1                   19.4

------------------------------------------------------------------------

Exercise 3
----------

Building on Exercise 2, plot median spend per individual for the five household sizes in `my_transaction_data`.

**Answer:**

``` r
exercise_2 %>% 
  group_by(hh_size) %>% 
  summarize(
    med_wkly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = hh_size, y = med_wkly_spend_per_ind)) +
  geom_col()
```

![](03-exploratory-data-analysis-solutions_files/figure-markdown_github/unnamed-chunk-6-1.png)

------------------------------------------------------------------------

Exercise 4
----------

Are baskets with diapers in them more likely than average to have beer in them too? Legend has it that placing these two product categories closer together can increase beer sales [(Powers 2002)](https://www.theregister.co.uk/2006/08/15/beer_diapers/). Using the following starter code, calculate [lift](https://en.wikipedia.org/wiki/Lift_(data_mining)) for the "association rule" that diapers in a basket (i.e., `sub_commodity_desc == 'BABY DIAPERS'`) imply beer is in the basket (i.e., `sub_commodity_desc == 'BEERALEMALT LIQUORS'`). Is the association between these products practically significant in `my_transaction_data`?

``` r
inner_join(my_transaction_data, product) %>% 
  mutate(
    diapers = sub_commodity_desc == 'BABY DIAPERS', 
    beer    = sub_commodity_desc == 'BEERALEMALT LIQUORS'
  )
```

**Answer:**

``` r
inner_join(my_transaction_data, product) %>% 
  mutate(
    diapers = sub_commodity_desc == 'BABY DIAPERS', 
    beer    = sub_commodity_desc == 'BEERALEMALT LIQUORS'
  ) %>%
  group_by(basket_id) %>%
  summarize(
    basket_has_diapers = max(diapers), 
    basket_has_beer    = max(beer)
  ) %>% 
  summarize(
    prop_both   = sum(basket_has_diapers * basket_has_beer == 1) / 
                  sum(basket_has_diapers == 1),
    prob_beer   = mean(basket_has_beer),
    diaper_lift = prop_both / prob_beer
  )
```

    ## # A tibble: 1 x 3
    ##   prop_both prob_beer diaper_lift
    ##       <dbl>     <dbl>       <dbl>
    ## 1    0.0602    0.0566        1.06

------------------------------------------------------------------------

Exercise 5
----------

Using a stacked bar chart that's partitioned by income level (i.e., `income_desc`), visualize the total amount of money customers spent on national-brand products versus private-label products.

**Answer:**

``` r
inner_join(my_transaction_data, hh_demographic) %>% 
  mutate(
    income_desc = factor(income_desc, 
                         levels = c('Under 15K',   '15-24K',   '25-34K', 
                                       '35-49K',   '50-74K',   '75-99K', 
                                     '100-124K', '125-149K', '150-174K', 
                                     '175-199K', '200-249K',    '250K+'),
                         ordered = TRUE)
  ) %>%
  group_by(income_desc, brand) %>%
  summarize(total_spend = sum(purchase_price)) %>% 
  ggplot() +
  geom_col(aes(x = income_desc, y = total_spend, fill = brand), 
           position = 'fill') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](03-exploratory-data-analysis-solutions_files/figure-markdown_github/unnamed-chunk-9-1.png)
