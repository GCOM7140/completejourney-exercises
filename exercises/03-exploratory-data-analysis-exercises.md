Exploratory Data Analysis (EDA) Exercises
================

The following seven exercises are based on concepts covered in [Chapter 7 of R4DS](http://r4ds.had.co.nz/exploratory-data-analysis.html). They can be answered using datasets from the `completejourney` package. Start by loading the `tidyverse` and `completejourney` packages.

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

------------------------------------------------------------------------

Exercise 3
----------

Building on Exercise 2, plot median spend per individual for the five household sizes in `my_transaction_data`.

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

------------------------------------------------------------------------

Exercise 5
----------

Using a stacked bar chart that's partitioned by income level (i.e., `income_desc`), visualize the total amount of money customers spent on national-brand products versus private-label products.
