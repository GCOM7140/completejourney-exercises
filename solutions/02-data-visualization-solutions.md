Data Visualization Solutions
================

The following questions are based on concepts covered in [Chapter 3](http://r4ds.had.co.nz/data-visualisation.html) of R4DS, and answers to them lie in the `transactions` and `products` datasets of the completejourney package. Load the tidyverse, completejourney, and lubridate packages to start working on them.

``` r
library(tidyverse)
library(completejourney)
library(lubridate) # see chapter 16 of r4ds
```

------------------------------------------------------------------------

**Question 1**: Explore the distribution of `quantity`. What, if anything, do you find unusual about this visualization?

We designed this question to strengthen your ability to visualize the distribution of a continuous variable with the `geom_histogram()` function.

``` r
ggplot(data = transactions) + 
  geom_histogram(mapping = aes(x = quantity))
```

![](02-data-visualization-solutions_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# The unusual aspect of the histogram is its extremely long tail. The distance
# is so far that the histogram almost appears to be a single bar at zero. This
# distortion warrants additional steps (e.g., `filter(quantity <= 10)`).

ggplot(data = transactions %>% filter(quantity <= 10)) + 
  geom_histogram(mapping = aes(x = quantity))
```

![](02-data-visualization-solutions_files/figure-markdown_github/unnamed-chunk-2-2.png)

------------------------------------------------------------------------

**Question 2**: Use a line graph to plot total sales value by date. What, if anything, do you find unusual about this visualization?

We designed this question to strengthen your ability to use dplyr verbs in combination with `geom_line()`.

You can get dates from the `transaction_timestamp` variable in the `transactions` dataset with the `date()` function of the lubridate package. If you are interested, you can see some examples of other accessor functions in the lubridate package [here](https://r4ds.had.co.nz/dates-and-times.html#date-time-components).

``` r
transactions %>% 
  mutate(date = date(transaction_timestamp)) %>% 
  group_by(date) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = date, y = total_sales_value))
```

![](02-data-visualization-solutions_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# The most extreme days of the year were November 23 and December 23,
# respectively. Sales on November 23 were likely very low due to it being Black
# Friday, and sales on December 23 were likely very high due to it being
# Christmas Eve.
```

------------------------------------------------------------------------

**Question 3**: Use a bar chart to compare the total sales value of national brands with that of private-label brands using the `brand` variable in the `products` dataset.

Because `transactions` does not contain product metadata, you will need to create a new dataset with product information in it. Consider running the code below for this purpose and using `transactions_products` for your answer.

``` r
transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% 
  mutate(brand  = fct_explicit_na(brand)) %>% 
  filter(brand != "(Missing)")
```

We designed this question to strengthen your ability to use dplyr verbs in combination with `geom_bar()` and its `stat` argument (see [here](https://r4ds.had.co.nz/data-visualisation.html#statistical-transformations)).

``` r
transactions_products %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(mapping = aes(x = brand, y = total_sales_value)) + 
  geom_bar(stat = "identity")
```

![](02-data-visualization-solutions_files/figure-markdown_github/unnamed-chunk-5-1.png)

------------------------------------------------------------------------

**Question 4**: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar chart to compare the split between national and private-label brands for soft drinks and cheeses.

Follow these three steps to create your plot:

-   Filter `transactions_products` to include only transactions with `product_category` equal to "SOFT DRINKS" or "CHEESE"
-   Calculate total sales value by `product_category` and `brand`
-   Create the bars using [`geom_col()`](https://jrnold.github.io/r4ds-exercise-solutions/data-visualisation.html#exercise-3.7.2) with `position = 'fill'`

``` r
transactions_products %>%
  filter(product_category %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(
    mapping  = aes(x = product_category, y = total_sales_value, fill = brand)
  ) + 
  geom_col(position = "fill")
```

![](02-data-visualization-solutions_files/figure-markdown_github/unnamed-chunk-6-1.png)

------------------------------------------------------------------------

**Question 5**: Filter `transactions_products` for transactions in the peanut better, jelly, and jams product category (i.e., `"PNT BTR/JELLY/JAMS"`). Then, create a bar chart to visualize the distribution of the retailer's PB&J transactions by package size. Which two package sizes are the most popular?

``` r
transactions_products %>% 
  filter(product_category == "PNT BTR/JELLY/JAMS") %>% 
  group_by(package_size) %>% 
  summarize(count = n()) %>% 
  ggplot() + 
  geom_bar(
    mapping = aes(x = package_size %>% fct_reorder(count), y = count), 
    stat    = "identity"
  ) +
  coord_flip()
```

![](02-data-visualization-solutions_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
# The most popular package size for PB&J products is 18 oz. The runner-up is 32
# oz.
```
