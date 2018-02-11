Data Transformation Solutions
================

The following 5 questions are based on concepts covered in Chapters 4-6 in R4DS and can be answered using The Complete Journey data. Start by loading the `tidyverse` and the `completejourney` package.

``` r
# required packages
library(tidyverse)
library(completejourney)
```

------------------------------------------------------------------------

**Question 1**: Using `transaction_data` create a 2-column dataset sorted by basket showing the most expensive item first. Hint: Sort by `basket_id` and then by `sales_value` and return only those two variables.
*This question grows your ability to use [`arrange()`](http://r4ds.had.co.nz/transform.html#arrange-rows-with-arrange) and [`select()`](http://r4ds.had.co.nz/transform.html#select-columns-with-select).*

**Answer**:

``` r
transaction_data %>%
  arrange(basket_id, desc(sales_value)) %>%
  select(basket_id, sales_value)
```

    ## # A tibble: 2,595,732 x 2
    ##      basket_id sales_value
    ##          <dbl>       <dbl>
    ##  1 26984851472       1.50 
    ##  2 26984851472       1.39 
    ##  3 26984851472       1.21 
    ##  4 26984851472       0.990
    ##  5 26984851472       0.820
    ##  6 26984851516       2.99 
    ##  7 26984851516       2.00 
    ##  8 26984851516       2.00 
    ##  9 26984851516       1.98 
    ## 10 26984851516       1.89 
    ## # ... with 2,595,722 more rows

------------------------------------------------------------------------

**Question 2**: In `transaction_data` add regular prices before loyalty and coupon discounts and the loyalty prices according to this logic:

-   `regular_price = (sales_value - (retail_disc + coupon_match_disc)) / quantity`
-   `loyalty_price = regular_price + (retail_disc / quantity)`

*This question grows your ability to use [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate).*

**Answer**:

``` r
transaction_data <- transaction_data %>%
  mutate(regular_price = (sales_value - (retail_disc + coupon_match_disc)) / quantity, 
         loyalty_price = regular_price + (retail_disc / quantity))
```

------------------------------------------------------------------------

**Question 3**: The `transaction_data` covers 92,339 unique product ids. How many unique products (not transactions!) had a regular price of one dollar or less? How is the count different for loyalty cardholders? Hint: After filtering select the product id column and count unique products using the `n_distinct()` function.
*This question grows your ability to use [`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter).*

**Answer**: Only 12,451 products had a regular price less than $1.00, but 19,966 products had a loyalty price of less than $1.00 indicating that over 7,500 products were discounted from regular price to below a dollar.

``` r
transaction_data %>% 
  filter(regular_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
```

    ## [1] 12451

``` r
transaction_data %>% 
  filter(loyalty_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()
```

    ## [1] 19966

------------------------------------------------------------------------

**Question 4**: What proportion of baskets were over $10 in sales value? Hint: You will need to summarize grouped by basket, `ungroup()` the data, and summarize over all baskets. In the last step you can calculate the proportion by taking the mean of `TRUE/FALSE` values. Use the code `mean(basket_value > 10)` to get the proportion over 10. *This question grows your ability to use [`summarize()`](http://r4ds.had.co.nz/transform.html#missing-values-1) and [`ungroup()`](http://r4ds.had.co.nz/transform.html#ungrouping).*

**Answer**: Approximately 65% of the baskets have a value over $10.

``` r
transaction_data %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion_under_10 = mean(basket_value > 10))
```

    ## # A tibble: 1 x 1
    ##   proportion_under_10
    ##                 <dbl>
    ## 1               0.654

------------------------------------------------------------------------

**Question 5**: Which store with over $10K in total sales value discounts their products the most for loyal customers? Hint: You can calculate the loyalty discount as a percentage of regular price using the following logic:

-   `pct_loyalty_disc = 1 - (loyalty_price / regular_price)`

*This question grows your ability to use `filter()`, `mutate()`, `group_by()`, `summarize()`, and `arrange()`; most everything covered in [Chapter 5](http://r4ds.had.co.nz/transform.html).*

**Answer**: `store_id` = 341 has, on average, a discount of 18.76% off regular price for members of its loyalty program. The code below returns all stores in rank order to give you a sense for how similar the discounting actually is between stores.

``` r
transaction_data %>%
  filter(is.finite(regular_price), is.finite(loyalty_price), regular_price > 0) %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  group_by(store_id) %>%
  summarize(total_sales_value = sum(sales_value), 
            avg_pct_loyalty_disc = mean(pct_loyalty_disc)) %>%
  filter(total_sales_value > 10000) %>%
  arrange(desc(avg_pct_loyalty_disc))
```

    ## # A tibble: 111 x 3
    ##    store_id total_sales_value avg_pct_loyalty_disc
    ##       <int>             <dbl>                <dbl>
    ##  1      341             44529                0.188
    ##  2      379             10848                0.177
    ##  3      306             52117                0.173
    ##  4      436             50735                0.157
    ##  5      403             61150                0.155
    ##  6    31401             34782                0.153
    ##  7      286             15485                0.152
    ##  8      311             61592                0.152
    ##  9      422             96515                0.152
    ## 10      443             70761                0.152
    ## # ... with 101 more rows
