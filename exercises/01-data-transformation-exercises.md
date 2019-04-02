Data Transformation Exercises
================

The following questions are based on concepts covered in [Chapter 5](http://r4ds.had.co.nz/transform.html) of R4DS, and answers to them lie in the `transactions` dataset of the completejourney package. Load the tidyverse and completejourney packages to start working on them.

``` r
library(tidyverse)
library(completejourney)
```

If printing the completjourney package's datasets to the console is throwing you a warning that 'along' is being autocompleted as 'along.with', you might want to toggle `warnPartialMatchArgs` off. Jenny Bryan writes that she has been doing this a lot of late [here](https://github.com/tidyverse/tidyr/issues/519#issuecomment-439148810). When the next version of R is released, the bug causing this error will be fixed. For now, you can toggle `warnPartialMatchArgs` off with:

``` r
options(warnPartialMatchArgs = FALSE)
```

------------------------------------------------------------------------

**Question 1**: Change the discount variables (i.e., `retail_disc`, `coupon_disc`, `coupon_match_disc`) from negative to positive.

Use the `abs()` function within [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate).

We designed this question to strengthen your ability to use the dplyr verb [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate) for the purposes of overwriting existing variables.

------------------------------------------------------------------------

**Question 2**: Create three new variables named `regular_price`, `loyalty_price`, and `coupon_price` according to the following logic:

We designed this question to strengthen your ability to use the dplyr verb [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate) to create new variables. It should also help you develop a better understanding of the discount variables in `transactions`.

------------------------------------------------------------------------

**Question 3**: The `transactions` dataset includes 68,509 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty price and coupon price?

After filtering, select the `product_id` column, then count the number of unique products using the `n_distinct()` function.

We designed this question to strengthen your ability to use the dplyr verbs [`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter) and [`select()`](http://r4ds.had.co.nz/transform.html#select-columns-with-select).

------------------------------------------------------------------------

**Question 4**: What proportion of baskets are over $10 in sales value?

You need to use [`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), and [`ungroup()`](http://r4ds.had.co.nz/transform.html#ungrouping). As a last step, you can calculate the proportion by taking the mean of `TRUE/FALSE` values, using `mean(basket_value > 10)` to get the proportion over $10.

We designed this question to strengthen your ability to use the dplyr verbs [`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise) and [`ungroup()`](http://r4ds.had.co.nz/transform.html#ungrouping).

------------------------------------------------------------------------

**Question 5**: Which store with over $10K in total `sales_value` discounts its products the most for loyal customers?

You can calculate loyalty discount as a percentage of regular price using the following logic:

We designed this question to strengthen your ability to use the dplyr verbs [`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter), [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate), [`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), and [`arrange()`](http://r4ds.had.co.nz/transform.html#arrange-rows-with-arrange) (i.e., almost everything covered in [Chapter 5](http://r4ds.had.co.nz/transform.html) of R4DS).
