Data Wrangling Solutions
================

The following 4 questions are based on concepts covered in Chapters 12-13 in R4DS and can be answered using The Complete Journey data. Start by loading the `tidyverse` and the `completejourney` package.

``` r
# required packages
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

**Answer**: Of the 1,584 households that were mailed coupons as part of a campaign, only 27.4% of redeemed a coupon.

``` r
hh_coupon_counts <- coupon_redempt %>% count(household_key)
hh_campaign_participants <- campaign_table %>% distinct(household_key)
hh_redemption_check <- left_join(hh_campaign_participants, 
                                 hh_coupon_counts, by='household_key')
mean(!is.na(hh_redemption_check$n))
```

    ## [1] 0.2739899

------------------------------------------------------------------------

**Question 2**: How many households did not redeem a coupon? Hint: Similar to Question 1, generate a unique list of households who participated in campaign and and another list who redeemed a coupon. Then use `anti_join()` to return a list of household campaign participants who are not in the redeemed dataset. Finally, count the rows of that dataset.
*This question tests how to count the non-existence of records between two tables using [`anti_join()`](http://r4ds.had.co.nz/relational-data.html#filtering-joins)*

**Answer**:

``` r
hh_coupon_counts <- coupon_redempt %>% count(household_key)
hh_campaign_participants <- campaign_table %>% distinct(household_key)

anti_join(hh_campaign_participants, 
          hh_coupon_counts, by='household_key') %>% nrow()
```

    ## [1] 1150

------------------------------------------------------------------------

**Question 3**: What was the most popular product bought with campaign coupons? Hint: First join the `coupon` and `product` to get the product description for the coupons, and then join with the `coupon_redempt`. The product is described in `sub_commodity_desc`.
*This question grows your ability to join data with [`inner_join()`](http://r4ds.had.co.nz/relational-data.html#inner-join) to guarantee existence in two datasets and then count on the merged dataset.*

**Answer**: The most popular product bought with a campaign coupon is "YOGURT NOT MULTI-PACKS".

``` r
coupon_data <- inner_join(coupon, 
                          product, by=c('product_id'))
product_redemption_data <- inner_join(coupon_redempt,
                                      coupon_data, by=c('campaign', 'coupon_upc'))

product_redemption_data %>%
  count(sub_commodity_desc) %>%
  arrange(desc(n)) %>%
  filter(row_number() <= 3)
```

    ## # A tibble: 3 x 2
    ##   sub_commodity_desc                 n
    ##   <chr>                          <int>
    ## 1 YOGURT NOT MULTI-PACKS         42631
    ## 2 FRZN SS PREMIUM ENTREES/DNRS/N 30971
    ## 3 CHOICE BEEF                    30016

------------------------------------------------------------------------

**Question 4**: Using the field `commodity_desc` determine which category spend grew the most? In order to calculate growth in sales, summarize the weekly spend per category then compare the first and last weeks. **Only consider categories with over $100 in total sales the first week**.

Here are some suggested steps:

1.  Join `transaction_data` with `product` to obtain the `commodity_desc` per transaction
2.  `group_by()` and `summarize()` the `sales_value` by `commodity_desc` and `week_no`
3.  Take the first and last weeks using `filter(row_number() == 1 | row_number() == n())`
4.  Re-group by category and create an indicator of the first and last week using `ifelse(row_number() == 1, 'first', 'last')`
5.  Unselect the `week_no` column and `spread()` the data to put the first and last columns side-by-side for comparison
6.  Calculate growth as `(last-first)/first`
7.  `filter()` to only categories with &gt;= 100 in `first`
8.  Finally, `arrange()` growth in descending order to determine the largest change

*This question grows your ability to create a pipeline of data transformation steps to generate results. Specific functions include: [`group_by()` and `summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter), [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate), [`select()`](http://r4ds.had.co.nz/transform.html#select-columns-with-select), [`spread()`](http://r4ds.had.co.nz/tidy-data.html#spreading), and [`arrange()`](http://r4ds.had.co.nz/transform.html#arrange-rows-with-arrange).*

**Answer**: "FLUID MILK PRODUCTS" had a first to last week change of over 16x, making it the category that grew the most.

``` r
transaction_data %>%
  left_join(product, by='product_id') %>%
  group_by(commodity_desc, week_no) %>%
  summarize(total_sales_value = sum(sales_value, na.rm=TRUE)) %>%
  arrange(commodity_desc, week_no) %>%
  group_by(commodity_desc) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  select(-week_no) %>%
  mutate(week_indicator = ifelse(row_number() == 1, 'first', 'last')) %>%
  spread(key = week_indicator, value = total_sales_value) %>%
  mutate(growth = (last-first) / first) %>%
  filter(first >= 100) %>%
  arrange(desc(growth))
```

    ## # A tibble: 10 x 4
    ## # Groups:   commodity_desc [10]
    ##    commodity_desc         first   last  growth
    ##    <chr>                  <dbl>  <dbl>   <dbl>
    ##  1 FLUID MILK PRODUCTS      118 2011    16.1  
    ##  2 FRZN MEAT/MEAT DINNERS   105 1631    14.6  
    ##  3 CHEESE                   114 1467    11.9  
    ##  4 BAKED BREAD/BUNS/ROLLS   103 1257    11.2  
    ##  5 BEEF                     188 2242    10.9  
    ##  6 SOFT DRINKS              272 3204    10.8  
    ##  7 BEERS/ALES               120 1098     8.13 
    ##  8 FROZEN PIZZA             172 1235     6.18 
    ##  9 CANDY - PACKAGED         121  658     4.46 
    ## 10 PARTY TRAYS              129   34.0 - 0.737
