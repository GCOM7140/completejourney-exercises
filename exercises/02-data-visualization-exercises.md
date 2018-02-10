Data Visualization Exercises
================

The following 5 questions are based on concepts covered in Chapters 1-3 in R4DS and can be answered using The Complete Journey data. Start by loading the `tidyverse` and the `completejourney` package.

``` r
library(tidyverse)
library(completejourney)
```

------------------------------------------------------------------------

**Question 1**: Create a histogram of quantity. Is there anything unusual in the graph?
*This question grows your ability to use `geom_histogram()`.*

------------------------------------------------------------------------

**Question 2**: Create a line chart that plots total sales value over time. Is there anything unusual in the graph?
*This question grows your ability to use `geom_line()`.*

------------------------------------------------------------------------

**Question 3**: Create a bar chart comparing total sales value of private label versus national brands. Assign different colors to the bars using the `fill` argument inside `aes()`.
*This question grows your ability to use `geom_bar()` along with its `stat` argument.*

------------------------------------------------------------------------

**Question 4**: Building upon Question 3, we suspect customers prefer national brands for soft drinks, but less so for diary products like cheese. Confirm this by creating a stacked bar chart showing the split of cheese sales between national and private brands and a similar split for soft drinks.

Hint: Follow these steps to create your plot:

-   Filter to only transactions with `commodity_desc` equal to "SOFT DRINKS" or "CHEESE"
-   Calculate the total sales value by `commodity_desc` and `brand`
-   Create the bars using `geom_bar` with `stat='identity'` and `position='fill'`

------------------------------------------------------------------------

**Question 5**: Below is a block of code that creates a dataset of transactions of peanut better, jelly and jams with the product size determined in ounces. Use the `pb_and_j_data` dataset to create a bar plot that shows the most popular size (in ounces) of peanut butter and jelly products.

``` r
my_transaction_data <- left_join(transaction_data, product, by='product_id')
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  select(curr_size_of_product) %>%
  mutate(product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)',
                                                  '\\1', curr_size_of_product))))
```
