Exploratory Data Analysis (EDA) Exercises
================
Zoe Zheng
April 10, 2018

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

    ## Joining, by = "product_id"

    ## Joining, by = "household_key"

------------------------------------------------------------------------

Exercise 1
----------

How many unique households exist in `my_transaction_data`, and how many of these households in `my_transaction_data` have demographic data in `hh_demographic`?

1.  Use `distinct()` to create a tibble of unique `household_key` values.
2.  Use `nrow()` to count these households.
3.  Use `inner_join()` to match `my_transaction_data` with `hh_demographic`.
4.  Use `distinct()` and `nrow()` to count the rows that remain.

<!-- -->

    ## [1] 2500

    ## Joining, by = c("household_key", "age_desc", "marital_status_code", "income_desc", "homeowner_desc", "hh_comp_desc", "household_size_desc", "kid_category_desc")

    ## [1] 801

------------------------------------------------------------------------

Exercise 2
----------

Determine median weekly spend per individual using the following tibble (i.e., `exercise_2`).

    ## Joining, by = c("household_key", "age_desc", "marital_status_code", "income_desc", "homeowner_desc", "hh_comp_desc", "household_size_desc", "kid_category_desc")

    ## # A tibble: 1 x 1
    ##   med_wkly_spend_per_ind
    ##                    <dbl>
    ## 1                   19.4

The median weekly spend per individual is $19.425.

------------------------------------------------------------------------

Exercise 3
----------

Building on Exercise 2, plot median spend per individual for the five household sizes in `my_transaction_data`.

![](03-exploratory-data-analysis-exercises-zheng-zoe_files/figure-markdown_github/question%203-1.png)

------------------------------------------------------------------------

Exercise 4
----------

Are baskets with diapers in them more likely than average to have beer in them too? Legend has it that placing these two product categories closer together can increase beer sales [(Powers 2002)](https://www.theregister.co.uk/2006/08/15/beer_diapers/). Using the following starter code, calculate [lift](https://en.wikipedia.org/wiki/Lift_(data_mining)) for the "association rule" that diapers in a basket (i.e., `sub_commodity_desc == 'BABY DIAPERS'`) imply beer is in the basket (i.e., `sub_commodity_desc == 'BEERALEMALT LIQUORS'`). Is the association between these products practically significant in `my_transaction_data`?

    ## Joining, by = c("product_id", "manufacturer", "department", "brand", "commodity_desc", "sub_commodity_desc", "curr_size_of_product")

    ## # A tibble: 2,581,266 x 31
    ##    household_key  basket_id   day product_id quantity sales_value store_id
    ##            <int>      <dbl> <int>      <int>    <int>       <dbl>    <int>
    ##  1          2375    2.70e10     1    1004906        1       1.39       364
    ##  2          2375    2.70e10     1    1033142        1       0.820      364
    ##  3          2375    2.70e10     1    1036325        1       0.990      364
    ##  4          2375    2.70e10     1    1082185        1       1.21       364
    ##  5          2375    2.70e10     1    8160430        1       1.50       364
    ##  6          2375    2.70e10     1     826249        2       1.98       364
    ##  7          2375    2.70e10     1    1043142        1       1.57       364
    ##  8          2375    2.70e10     1    1085983        1       2.99       364
    ##  9          2375    2.70e10     1    1102651        1       1.89       364
    ## 10          2375    2.70e10     1    6423775        1       2.00       364
    ## # ... with 2,581,256 more rows, and 24 more variables: retail_disc <dbl>,
    ## #   trans_time <chr>, week_no <int>, coupon_disc <dbl>,
    ## #   coupon_match_disc <dbl>, manufacturer <int>, department <chr>,
    ## #   brand <chr>, commodity_desc <chr>, sub_commodity_desc <chr>,
    ## #   curr_size_of_product <chr>, age_desc <chr>, marital_status_code <chr>,
    ## #   income_desc <chr>, homeowner_desc <chr>, hh_comp_desc <chr>,
    ## #   household_size_desc <chr>, kid_category_desc <chr>,
    ## #   regular_price <dbl>, loyalty_price <dbl>, coupon_price <dbl>,
    ## #   purchase_price <dbl>, diapers <lgl>, beer <lgl>

    ## Joining, by = c("product_id", "manufacturer", "department", "brand", "commodity_desc", "sub_commodity_desc", "curr_size_of_product")

    ## # A tibble: 1 x 3
    ##   prop_both prob_beer diaper_lift
    ##       <dbl>     <dbl>       <dbl>
    ## 1    0.0602    0.0566        1.06

diaper\_lift&gt;1, meaning that the probability of buying both diapers and beer is higher than probability of just buying beer. So, there is a significant association between these products.

------------------------------------------------------------------------

Exercise 5
----------

Using a stacked bar chart that's partitioned by income level (i.e., `income_desc`), visualize the total amount of money customers spent on national-brand products versus private-label products.

    ## Joining, by = c("household_key", "age_desc", "marital_status_code", "income_desc", "homeowner_desc", "hh_comp_desc", "household_size_desc", "kid_category_desc")

![](03-exploratory-data-analysis-exercises-zheng-zoe_files/figure-markdown_github/question%205-1.png)

Overall, consumers across all income level spend more on national-brand products than private-label products.
