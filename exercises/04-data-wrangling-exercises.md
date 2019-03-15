Data Wrangling Exercises
================

The following questions are based on concepts covered in [Chapter 12](http://r4ds.had.co.nz/tidy-data.html) and [Chapter 13](http://r4ds.had.co.nz/relational-data.html) of R4DS, and answers to them lie in the `coupons`, `coupon_redemptions`, `campaigns`, `transactions`, and `products` datasets of the completejourney package. Load the tidyverse, completejourney, and lubridate packages to start working on them.

``` r
library(tidyverse)
library(completejourney)
library(lubridate) # see chapter 16 of r4ds
```

------------------------------------------------------------------------

**Question 1**: What percent of households that received the retailer's weekly mailer redeemed at least one coupon?

------------------------------------------------------------------------

**Question 2**: How many households received and did not redeem a coupon?

------------------------------------------------------------------------

**Question 3**: What percentage of coupons promoted in the retailer's weekly mailer got redeemed at least once?

------------------------------------------------------------------------

**Question 4**: Considering the product categories that the 801 households in the Complete Journey Study purchased most heavily, which five categories did they start spending more on at the highest rate over the course of Q1? Only consider product categories that the group spent $1,500 or more on in January, and calculate spend growth as a percentage of category spend in January.

Here are some suggestions:
1. Join the `transactions` and `products` datasets.
2. Get month from `transaction_timestamp` and name this variable `month`.
3. Group by `product_category` and `month`.
4. Calculate total spend at the category-month level using `sales_value`.
5. Filter the data to include only the months of January and March.
6. Create a new variable called `spend_growth_pct`, making use of the [`lag()`](https://r4ds.had.co.nz/transform.html#mutate-funs) function.
7. Filter for categories that had category spend of $1,500 or more in January.
8. Arrange the data in descending order according to `spend_growth_pct`.
