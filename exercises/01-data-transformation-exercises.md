Data Transformation Exercises
================

The following five questions are based on concepts covered in Chapters 4-6 of 
R4DS. Answer them using the Complete Journey data and start by loading the 
`tidyverse` and `completejourney` packages.

``` r
library(tidyverse)
library(completejourney)
```

------------------------------------------------------------------------

**Question 1**: Using `transaction_data`, create a two-column tibble sorted by 
basket, showing the most expensive item first. **Hint:** Sort by `basket_id` and 
then by `sales_value`. Return only those two variables. This question is meant 
to strengthen your ability to use the `dplyr` verbs [`arrange()`][arrange] and 
[`select()`][select].

[arrange]: http://r4ds.had.co.nz/transform.html#arrange-rows-with-arrange
[select]: http://r4ds.had.co.nz/transform.html#select-columns-with-select

------------------------------------------------------------------------

**Question 2**: Create new variables for `regular price` and `loyalty price` 
according to the following logic:

-   `regular_price = (sales_value - (retail_disc + coupon_match_disc)) / quantity`
-   `loyalty_price = regular_price + (retail_disc / quantity)`

This question is meant to strengthen your ability to use the `dplyr` verb 
[`mutate()`][mutate].

[mutate]: http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate

------------------------------------------------------------------------

**Question 3**: The `transaction_data` covers 92,339 unique product ids. How many unique products (not transactions!) had a regular price of one dollar or less? How is the count different for loyalty cardholders? Hint: After filtering select the product id column and count unique products using the `n_distinct()` function.
*This question grows your ability to use [`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter).*

------------------------------------------------------------------------

**Question 4**: What proportion of baskets were over $10 in sales value? Hint: You will need to summarize grouped by basket, `ungroup()` the data, and summarize over all baskets. In the last step you can calculate the proportion by taking the mean of `TRUE/FALSE` values. Use the code `mean(basket_value > 10)` to get the proportion over 10. *This question grows your ability to use [`summarize()`](http://r4ds.had.co.nz/transform.html#missing-values-1) and [`ungroup()`](http://r4ds.had.co.nz/transform.html#ungrouping).*

------------------------------------------------------------------------

**Question 5**: Which store with over $10K in total sales value discounts their products the most for loyal customers? Hint: You can calculate the loyalty discount as a percentage of regular price using the following logic:

-   `pct_loyalty_disc = 1 - (loyalty_price / regular_price)`

*This question grows your ability to use `filter()`, `mutate()`, `group_by()`, `summarize()`, and `arrange()`; most everything covered in [Chapter 5](http://r4ds.had.co.nz/transform.html).*
