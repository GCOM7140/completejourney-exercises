Data Wrangling Exercises
================

The following 4 questions are based on concepts covered in Chapters 12-13 in R4DS and can be answered using The Complete Journey data. Start by loading the `tidyverse` and the `completejourney` package.

``` r
library(tidyverse)
library(completejourney)
```

------------------------------------------------------------------------

**Question 1**: What percentage of households redeemed at least 1 coupon from a mailed campaign? Hint: There are a few different ways to answer this question using joins. One possible solution is to:

1.  Summarize the number of coupon redemptions for each household using `coupon_redempt`
2.  Use `distinct()` on the `campaign_table` to produce a list of households sent a campaign
3.  Use `left_join()` to match households sent a campaign (derived in Step 2) with ones that redeemed a coupon (derived in Step 1)
4.  Determine the percentage of observations with non-missing counts meaning the join found a match in the summary of households with redeemed coupons

*This question grows your ability to produce counts and statistics by using [`left_join()`](http://r4ds.had.co.nz/relational-data.html#outer-join) and examining resulting the `NA` records.*

------------------------------------------------------------------------

**Question 2**: How many households did not redeem a coupon? Hint: Similar to Question 1, generate a unique list of households who participated in campaign and and another list who redeemed a coupon. Then use `anti_join()` to return a list of household campaign participants who are not in the redeemed dataset. Finally, count the rows of that dataset.
*This question tests how to count the non-existence of records between two tables using [`anti_join()`](http://r4ds.had.co.nz/relational-data.html#filtering-joins)*

------------------------------------------------------------------------

**Question 3**: What was the most popular product bought with campaign coupons? Hint: First join the `coupon` and `product` to get the product description for the coupons, and then join with the `coupon_redempt`. The product is described in `sub_commodity_desc`.
*This question grows your ability to join data with [`inner_join()`](http://r4ds.had.co.nz/relational-data.html#inner-join) to guarantee existence in two datasets and then count on the merged dataset.*

------------------------------------------------------------------------

**Question 4**: Using the field `commodity_desc` determine which category spend grew the most? In order to calculate growth in sales, summarize the weekly spend per category then compare the first and last weeks. **Only consider categories with over $100 in total sales the first week**.

Here are some suggested steps:

1.  `group_by()` and `summarize()` the `sales_value` by `commodity_desc` and `week_no`
2.  Take the first and last weeks using `filter(row_number() == 1 | row_number() == n())`
3.  Re-group by category and create an indicator of the first and last week using `ifelse(row_number() == 1, 'first', 'last')`
4.  Unselect the `week_no` column and `spread()` the data to put the first and last columns side-by-side for comparison
5.  Calculate growth as `(last-first)/first`
6.  `filter()` to only categories with &gt;= 100 in `first`
7.  Finally, `arrange()` growth in descending order to determine the largest change

*This question grows your ability to create a pipeline of data transformation steps to generate results. Specific functions include: [`group_by()` and `summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter), [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate), [`select()`](http://r4ds.had.co.nz/transform.html#select-columns-with-select), [`spread()`](http://r4ds.had.co.nz/tidy-data.html#spreading), and [`arrange()`](http://r4ds.had.co.nz/transform.html#arrange-rows-with-arrange).*
