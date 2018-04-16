Data Wrangling Exercises
================

The following four exercises are based on concepts covered in [Chapter 12](http://r4ds.had.co.nz/tidy-data.html) and [Chapter 13](http://r4ds.had.co.nz/relational-data.html) of [R for Data Science](http://r4ds.had.co.nz/). Use the `coupon`, `coupon_redempt`, `campaign_table`, `transaction_data`, and `product` datasets that come with the `completejourney` package to work on them, and start by loading the `tidyverse` and `completejourney` packages.

``` r
library(tidyverse)
library(completejourney)
```

------------------------------------------------------------------------

Exercise 1
----------

What percent of households that received the retailer's weekly mailer redeemed at least one coupon?

------------------------------------------------------------------------

Exercise 2
----------

How many households did not redeem a coupon?

------------------------------------------------------------------------

Exercise 3
----------

What percent of coupons promoted in the retailer's weekly mailer got redeemed at least once?

------------------------------------------------------------------------

Exercise 4
----------

Using the `transaction_data` and `product` datasets, determine which product category (i.e., `sub_commodity_desc`) grew the most in terms of revenue for the retailer in the second half of the study period (i.e., between `week_no == 52` and `week_no == 102`). Only consider product categories that had over $100 in revenue in week 52, and calculate revenue growth as a percentage of category revenue in week 52.

Here are some suggested steps:

1.  Join the `transaction_data` and `product` datasets.
2.  Group the data by `sub_commodity_desc` and `week_no`.
3.  Calculate `sales_value` at the category level.
4.  Filter the data to only include category revenues in weeks 52 and 102.
5.  Create a new variable called `revenue_growth`, making use of the `lag()` function. Recognize that you only want to consider categories that had category revenues of at least $100 in week 52.
6.  Arrange the data in descending order according to `revenue_growth`.
