Data Transformation Solutions
================

The following five questions are based on concepts covered in [Chapter 5](http://r4ds.had.co.nz/transform.html) of [R4DS](http://r4ds.had.co.nz/). Answer them using `transaction_data` in the Complete Journey data package and start by loading the `tidyverse` and `completejourney` packages.

``` r
library(tidyverse)
library(completejourney)

transaction_data <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time
  )
```

------------------------------------------------------------------------

**Question 1**: Change the discount variables (i.e., `retail_disc`, `coupon_disc`, `coupon_match_disc`) from negative to positive.

**Hint:** Use the `abs()` function within [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate).

This question is designed to strengthen your ability to use the `dplyr` verb [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate) to overwrite an existing variable.

**Answer**:

``` r
transaction_data <- transaction_data %>% 
  mutate(
    retail_disc       = abs(retail_disc),
    coupon_disc       = abs(coupon_disc),
    coupon_match_disc = abs(coupon_match_disc)
  )
```

------------------------------------------------------------------------

**Question 2**: Create three new variables named `regular_price`, `loyalty_price`, and `coupon_price` according to the following logic:

``` r
regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity
loyalty_price = (sales_value + coupon_match_disc) / quantity
coupon_price  = (sales_value - coupon_disc) / quantity
```

This question is designed to strengthen your ability to use the `dplyr` verb [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate) to create new variables from existing ones. It should also help you develop a better understanding of the discount variables in `transaction_data`.

**Answer:**

``` r
(transaction_data <- transaction_data %>% 
  mutate(
    regular_price     = (sales_value + retail_disc + coupon_match_disc) / 
                        quantity,
    loyalty_price     = (sales_value + coupon_match_disc) / 
                        quantity,
    coupon_price      = (sales_value - coupon_disc) / 
                        quantity
  ) %>% 
  select(regular_price, loyalty_price, coupon_price, everything())
)
```

    ## # A tibble: 2,595,732 x 15
    ##    regular_price loyalty_price coupon_price quantity sales_value
    ##            <dbl>         <dbl>        <dbl>    <int>       <dbl>
    ##  1         1.99          1.39         1.39         1       1.39 
    ##  2         0.820         0.820        0.820        1       0.820
    ##  3         1.29          0.990        0.990        1       0.990
    ##  4         1.21          1.21         1.21         1       1.21 
    ##  5         1.89          1.50         1.50         1       1.50 
    ##  6         1.29          0.990        0.990        2       1.98 
    ##  7         2.25          1.57         1.57         1       1.57 
    ##  8         3.39          2.99         2.99         1       2.99 
    ##  9         1.89          1.89         1.89         1       1.89 
    ## 10         2.79          2.00         2.00         1       2.00 
    ## # ... with 2,595,722 more rows, and 10 more variables: retail_disc <dbl>,
    ## #   coupon_disc <dbl>, coupon_match_disc <dbl>, household_key <int>,
    ## #   store_id <int>, basket_id <dbl>, product_id <int>, week_no <int>,
    ## #   day <int>, trans_time <chr>

------------------------------------------------------------------------

**Question 3**: `transaction_data` includes 92,339 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty and coupon prices?

**Hint:** After filtering, select the `product_id` column and count unique products using the `n_distinct()` function.

This question is designed to strengthen your ability to use the `dplyr` verbs [`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter) and [`select()`](http://r4ds.had.co.nz/transform.html#select-columns-with-select).

**Answer:**

12,442 products had a regular price less than or equal to $1.00. This count for loyalty price is 20,113 products. For coupon price, it's 22,273. These numbers indicate that over 7,500 products were discounted to a dollar or less with loyalty-card promotions or coupons.

``` r
transaction_data %>% 
  filter(regular_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
```

    ## [1] 12442

``` r
transaction_data %>% 
  filter(loyalty_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
```

    ## [1] 20113

``` r
transaction_data %>% 
  filter(coupon_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
```

    ## [1] 22273

------------------------------------------------------------------------

**Question 4**: What proportion of baskets are over $10 in sales value?

**Hint:** You need to use [`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), and [`ungroup()`](http://r4ds.had.co.nz/transform.html#ungrouping). Summarize over all baskets. In the last step, you can calculate the proportion by taking the mean of `TRUE/FALSE` values. Use `mean(basket_value > 10)` to get the proportion over $10.

This question is designed to strengthen your ability to use the `dplyr` verbs [`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise) and [`ungroup()`](http://r4ds.had.co.nz/transform.html#ungrouping).

**Answer:**

Approximately 65% of baskets bring the retailer over $10 in value.

``` r
transaction_data %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion_over_10 = mean(basket_value > 10))
```

    ## # A tibble: 1 x 1
    ##   proportion_over_10
    ##                <dbl>
    ## 1              0.654

------------------------------------------------------------------------

**Question 5**: Which store with over $10K in total `sales_value` discounts its products the most for loyal customers?

**Hint:** You can calculate loyalty discount as a percentage of regular price using the following logic:

``` r
pct_loyalty_disc = 1 - (loyalty_price / regular_price)
```

This question is designed to strengthen your ability to use the `dplyr` verbs [`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter), [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate), [`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), and [`arrange()`](http://r4ds.had.co.nz/transform.html#arrange-rows-with-arrange) (i.e., almost everything covered in [Chapter 5](http://r4ds.had.co.nz/transform.html).

**Answer:**

`store_id == 341` has, on average, a discount of 18.8% off regular price for members of the retailer's loyalty program. The code below returns all stores in rank order to give a sense of how similar the discounting is (not) between stores.

``` r
transaction_data %>%
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

    ## # A tibble: 111 x 3
    ##    store_id total_sales_value avg_pct_loyalty_disc
    ##       <int>             <dbl>                <dbl>
    ##  1      341            44529.                0.188
    ##  2      379            10848.                0.177
    ##  3      306            52117.                0.173
    ##  4      436            50735.                0.157
    ##  5      403            61150.                0.155
    ##  6    31401            34782.                0.153
    ##  7      286            15485.                0.152
    ##  8      311            61592.                0.152
    ##  9      422            96515.                0.152
    ## 10      443            70761.                0.152
    ## # ... with 101 more rows
