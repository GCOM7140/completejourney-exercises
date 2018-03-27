
library(tidyverse)
library(completejourney)
library(dplyr)

transaction_data <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time
  )


------------------------------------------------------------------------

**Question 1**: Change the discount variables (i.e., `retail_disc`, `coupon_disc`, `coupon_match_disc`) from negative to positive.

mutate(transaction_data,retail_disc=abs(retail_disc),coupon_disc=abs(coupon_disc),coupon_match_disc=abs(coupon_match_disc))


------------------------------------------------------------------------

**Question 2**: Create three new variables named `regular_price`, `loyalty_price`, and `coupon_price` according to the following logic:

``` r
regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity
loyalty_price = (sales_value + coupon_match_disc) / quantity
coupon_price  = (sales_value - coupon_disc) / quantity

transaction_data %>%
mutate(retail_disc=abs(retail_disc),coupon_disc=abs(coupon_disc),coupon_match_disc=abs(coupon_match_disc)) %>%
mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity) %>%
mutate(loyalty_price =(sales_value + coupon_match_disc) / quantity) %>%
mutate(coupon_price  = (sales_value - coupon_disc) / quantity)
```

This question is designed to strengthen your ability to use the `dplyr` verb [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate) to create new variables from existing ones. It should also help you develop a better understanding of the discount variables in `transaction_data`.

------------------------------------------------------------------------

**Question 3**: `transaction_data` includes 92,339 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty and coupon prices?

#for regular price
transaction_data %>%
mutate(retail_disc=abs(retail_disc),coupon_disc=abs(coupon_disc),coupon_match_disc=abs(coupon_match_disc)) %>%
mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity) %>%
mutate(loyalty_price =(sales_value + coupon_match_disc) / quantity) %>%
mutate(coupon_price  = (sales_value - coupon_disc) / quantity) %>%
select(regular_price, loyalty_price, coupon_price,everything()) %>%
filter(regular_price<=1) %>%
select(product_id)%>%
mutate(distinct=n_distinct(product_id))

#for loyalty price

#for coupon price

**Hint:** After filtering, select the product id column and count unique products using the `n_distinct()` function.

This question is designed to strengthen your ability to use the `dplyr` verbs [`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter) and [`select()`](http://r4ds.had.co.nz/transform.html#select-columns-with-select).

------------------------------------------------------------------------

**Question 4**: What proportion of baskets are over $10 in sales value?


group_by(transaction_data,basket_id)%>%
summarise(basket_value=sum(sales_value))%>%
mutate(proportion=mean(basket_value>10))

**Hint:** You need to use [`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), and [`ungroup()`](http://r4ds.had.co.nz/transform.html#ungrouping). Summarize over all baskets. In the last step, you can calculate the proportion by taking the mean of `TRUE/FALSE` values. Use `mean(basket_value > 10)` to get the proportion over $10.

This question is designed to strengthen your ability to use the `dplyr` verbs [`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise) and [`ungroup()`](http://r4ds.had.co.nz/transform.html#ungrouping).

------------------------------------------------------------------------

**Question 5**: Which store with over $10K in total `sales_value` discounts its products the most for loyal customers?


transaction_data %>%
mutate(retail_disc=abs(retail_disc),coupon_disc=abs(coupon_disc),coupon_match_disc=abs(coupon_match_disc)) %>%
mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity) %>%
mutate(loyalty_price =(sales_value + coupon_match_disc) / quantity) %>%
mutate(pct_loyalty_disc=1-(loyalty_price/regular_price))%>%
arrange(desc(pct_loyalty_disc))
group_by(store_id)%>%
summarise(total_sales=sum(sales_value))%>%
filter(total_sales>10000)%>%
arrange(desc(pct_loyalty_disc))

**Hint:** You can calculate loyalty discount as a percentage of regular price using the following logic:

``` r
pct_loyalty_disc = 1 - (loyalty_price / regular_price)
```

This question is designed to strengthen your ability to use the `dplyr` verbs [`filter()`](http://r4ds.had.co.nz/transform.html#filter-rows-with-filter), [`mutate()`](http://r4ds.had.co.nz/transform.html#add-new-variables-with-mutate), [`group_by()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), [`summarize()`](http://r4ds.had.co.nz/transform.html#grouped-summaries-with-summarise), and [`arrange()`](http://r4ds.had.co.nz/transform.html#arrange-rows-with-arrange) (i.e., almost everything covered in [Chapter 5](http://r4ds.had.co.nz/transform.html).
