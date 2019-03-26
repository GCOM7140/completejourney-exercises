Exploratory Data Analysis (EDA) Exercises
================

The following questions are based on concepts covered in
[Chapter 7](http://r4ds.had.co.nz/exploratory-data-analysis.html) of
R4DS, and answers to them lie in the `transactions`, `demographics`, and
`products` datasets of the completejourney package. Load the tidyverse
and completejourney packages to start working on them.

``` r
library(tidyverse)
library(completejourney)
```

With these packages loaded, begin by creating `transactions_prices`.

``` r
transactions %>% 
  filter(quantity != 0) %>%
  mutate(
     price_regular  = (sales_value + retail_disc + coupon_match_disc) /
                       quantity,
     price_loyalty  = (sales_value + coupon_match_disc) / 
                       quantity,
     price_coupon   = (sales_value - coupon_disc) / 
                       quantity,
     price_purchase = case_when(
                            coupon_disc > 0 ~ price_coupon, 
                            retail_disc > 0 ~ price_loyalty,
                            TRUE            ~ price_regular
       )
  ) -> 
  transactions_prices
```

-----

**Question 1**: Determine median weekly spend per individual (not
household) using `price_purchase` in`transactions_prices` and
`household_size` in `demographics`.

Because `transactions_prices` does not contain household metadata, you
need to create a new dataset with household information in it. In
addition, because `household_size` is a factor variable, you need to
convert it to an integer variable to calculate weekly spend per
individual. Consider using the code below for the beginning of the pipe
you build for this question.

``` r
transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
                     as.integer()
  )
```

-----

**Question 2**: Building on Question 2, plot median spend per individual
by household size.

-----

**Question 3**: Are baskets with diapers in them more likely than
average to have beer in them, too? Legend has it that placing these two
product categories closer together can increase beer sales
([Powers 2002](https://www.theregister.co.uk/2006/08/15/beer_diapers/)).
Using the following starter code, calculate
[lift](https://en.wikipedia.org/wiki/Lift_\(data_mining\)) for the
“association rule” that diapers in a basket (i.e., `product_type ==
"BABY DIAPERS"`) imply that beer is in the basket (i.e., `product_type
== "BEERALEMALT LIQUORS"`). Does the association between these products
offer support for the legend?

``` r
transactions_prices %>% 
  inner_join(products, by = "product_id") %>% 
  mutate(
    diapers = product_type == "BABY DIAPERS", 
    beer    = product_type == "BEERALEMALT LIQUORS"
  )
```

-----

**Question 4**: Using a stacked bar chart that is partitioned by income
level (i.e., `income`), visualize the total amount of money that
households in the Complete Journey Study spent on national-brand
products versus private-label products (i.e., `brand`).

Because `transactions_prices` does not contain household or product
metadata, you need to create a new dataset with this information in it.
Consider using the code below for the beginning of the pipe you build
for this question.

``` r
transactions_prices %>% 
  left_join(demographics, by = "household_id") %>% 
  left_join(products, by = "product_id")
```
