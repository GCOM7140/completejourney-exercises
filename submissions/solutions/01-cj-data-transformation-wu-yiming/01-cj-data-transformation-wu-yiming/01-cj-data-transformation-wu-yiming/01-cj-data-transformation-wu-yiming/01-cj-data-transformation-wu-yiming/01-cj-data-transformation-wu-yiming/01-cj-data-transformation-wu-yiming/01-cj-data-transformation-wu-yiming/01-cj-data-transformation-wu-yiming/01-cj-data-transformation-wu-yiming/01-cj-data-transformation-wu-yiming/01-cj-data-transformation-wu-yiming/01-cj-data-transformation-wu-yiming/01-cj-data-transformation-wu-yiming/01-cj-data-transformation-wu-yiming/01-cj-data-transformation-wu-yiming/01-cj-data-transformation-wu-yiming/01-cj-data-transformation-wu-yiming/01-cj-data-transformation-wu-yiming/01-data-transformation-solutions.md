Data Transformation Solutions
================

The following questions are based on concepts covered in
[Chapter 5](http://r4ds.had.co.nz/transform.html) of R4DS, and answers
to them lie in the `transactions` dataset of the completejourney
package. Load the tidyverse and completejourney packages to start
working on them.

``` r
library(tidyverse)
library(completejourney)
```

**Note**: If printing the completjourney package’s datasets to the
console is throwing you a warning that ‘along’ is being autocompleted as
‘along.with’, you might want to toggle `warnPartialMatchArgs` off. Jenny
Bryan writes that she has been doing this a lot of late
[here](https://github.com/tidyverse/tidyr/issues/519#issuecomment-439148810).
When the next version of R is released, the bug causing this error will
be fixed. For now, you can toggle `warnPartialMatchArgs` off with:

``` r
options(warnPartialMatchArgs = FALSE)
```

-----

**Question 1**: Change the discount variables (i.e., `retail_disc`,
`coupon_disc`, `coupon_match_disc`) from negative to positive.

**Hint:** Use the `abs()` function within
[`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate).

We designed this question to strengthen your ability to use the dplyr
verb
[`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate)
for the purposes of overwriting existing variables.

``` r
transactions <- transactions %>% 
  mutate(
    retail_disc       = abs(retail_disc),
    coupon_disc       = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )
```

-----

**Question 2**: Create three new variables named `regular_price`,
`loyalty_price`, and `coupon_price` according to the following
logic:

``` r
regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity
loyalty_price = (sales_value + coupon_match_disc) / quantity
coupon_price  = (sales_value - coupon_disc) / quantity
```

We designed this question to strengthen your ability to use the dplyr
verb
[`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate)
to create new variables. It should also help you develop a better
understanding of the discount variables in `transactions`.

``` r
(transactions <- transactions %>% 
  mutate(
    regular_price = (sales_value + retail_disc + coupon_match_disc) / 
                     quantity,
    loyalty_price = (sales_value + coupon_match_disc) / 
                     quantity,
    coupon_price  = (sales_value - coupon_disc) / 
                     quantity
  )
)
```

    ## # A tibble: 1,469,307 x 14
    ##    household_id store_id basket_id product_id quantity sales_value
    ##    <chr>        <chr>    <chr>     <chr>         <dbl>       <dbl>
    ##  1 900          330      31198570… 1095275           1        0.5 
    ##  2 900          330      31198570… 9878513           1        0.99
    ##  3 1228         406      31198655… 1041453           1        1.43
    ##  4 906          319      31198705… 1020156           1        1.5 
    ##  5 906          319      31198705… 1053875           2        2.78
    ##  6 906          319      31198705… 1060312           1        5.49
    ##  7 906          319      31198705… 1075313           1        1.5 
    ##  8 1058         381      31198676… 985893            1        1.88
    ##  9 1058         381      31198676… 988791            1        1.5 
    ## 10 1058         381      31198676… 9297106           1        2.69
    ## # … with 1,469,297 more rows, and 8 more variables: retail_disc <dbl>,
    ## #   coupon_disc <dbl>, coupon_match_disc <dbl>, week <int>,
    ## #   transaction_timestamp <dttm>, regular_price <dbl>,
    ## #   loyalty_price <dbl>, coupon_price <dbl>

-----

**Question 3**: The `transactions` dataset includes 68,509 unique
product IDs. How many of these products (not transactions\!) had a
regular price of one dollar or less? What does this count equal for
loyalty price and coupon price?

**Hint:** After filtering, select the `product_id` column, then count
the number of unique products using the `n_distinct()` function.

We designed this question to strengthen your ability to use the dplyr
verbs
[`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter)
and
[`select()`](http://r4ds.had.co.nz/transform.html#select-columns-with-select).

``` r
transactions %>% 
  filter(regular_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
```

    ## [1] 8698

``` r
# 8,698 products had a regular price less than or equal to $1.00.

transactions %>% 
  filter(loyalty_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
```

    ## [1] 14043

``` r
# This count for loyalty price is 14,043 products.

transactions %>% 
  filter(coupon_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
```

    ## [1] 15676

``` r
# For coupon price, it's higher still at 15,676. 

# These numbers indicate that over 5,300 products were discounted to a dollar or
# less with loyalty-card promotions or coupons.
```

-----

**Question 4**: What proportion of baskets are over $10 in sales value?

**Hint:** You need to use
[`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise),
[`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise),
and [`ungroup()`](http://r4ds.had.co.nz/transform.html#ungrouping). As a
last step, you can calculate the proportion by taking the mean of
`TRUE/FALSE` values, using `mean(basket_value > 10)` to get the
proportion over $10.

We designed this question to strengthen your ability to use the dplyr
verbs
[`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise),
[`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise)
and [`ungroup()`](http://r4ds.had.co.nz/transform.html#ungrouping).

``` r
transactions %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion_over_10 = round(mean(basket_value > 10) * 100, 0))
```

    ## # A tibble: 1 x 1
    ##   proportion_over_10
    ##                <dbl>
    ## 1                 66

``` r
# Approximately 66% of customers' baskets bring the retailer over $10 in value.
```

-----

**Question 5**: Which store with over $10K in total `sales_value`
discounts its products the most for loyal customers?

**Hint:** You can calculate loyalty discount as a percentage of regular
price using the following logic:

``` r
pct_loyalty_disc = 1 - (loyalty_price / regular_price)
```

We designed this question to strengthen your ability to use the dplyr
verbs
[`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter),
[`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate),
[`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise),
[`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise),
and
[`arrange()`](http://r4ds.had.co.nz/transform.html#arrange-rows-with-arrange)
(i.e., almost everything covered in
[Chapter 5](http://r4ds.had.co.nz/transform.html) of R4DS).

``` r
transactions %>%
  filter(
    is.finite(regular_price), 
    is.finite(loyalty_price), 
    regular_price > 0
  ) %>%
  mutate(
    pct_loyalty_disc     = 1 - (loyalty_price / regular_price)
  ) %>%
  group_by(store_id) %>%
  summarize(
    total_sales_value    = sum(sales_value), 
    avg_pct_loyalty_disc = mean(pct_loyalty_disc)
  ) %>%
  filter(total_sales_value > 10000) %>%
  arrange(desc(avg_pct_loyalty_disc))
```

    ## # A tibble: 106 x 3
    ##    store_id total_sales_value avg_pct_loyalty_disc
    ##    <chr>                <dbl>                <dbl>
    ##  1 341                 24163.                0.187
    ##  2 306                 27890.                0.164
    ##  3 436                 29880.                0.157
    ##  4 31401               20265.                0.154
    ##  5 403                 32507.                0.153
    ##  6 422                 58873.                0.152
    ##  7 288                 21336.                0.152
    ##  8 370                 31398.                0.151
    ##  9 311                 37335.                0.150
    ## 10 443                 38471.                0.149
    ## # … with 96 more rows

``` r
# store_id == 341 has, on average, a discount of 18.7% off regular prices for
# members of the retailer's loyalty program. The code below returns all stores
# in rank order to give a sense of how (dis)similar the discounting is between
# stores.
```
