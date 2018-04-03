Data Visualization Exercises
================
Cara Salyers
April 3, 2018

The following 5 questions are based on concepts covered in Chapters 1-3 in R4DS and can be answered using The Complete Journey data. Start by loading the `tidyverse` and the `completejourney` package.

``` r
library(tidyverse)
library(completejourney)
```

------------------------------------------------------------------------

**Question 1**: Create a histogram of quantity. Is there anything unusual in the graph?
*This question grows your ability to use `geom_histogram()`.*

**ANSWER:** The plot is unusual in that there is an extremely large count for the quantity of zero, so much so, that it is making any other values extremely difficult to see. There is also a very large quantity range indicating that there may be a value with a very high quantity that is causing the plot dimensions to be skewed/hard to see.

``` r
ggplot(data = transaction_data, aes(quantity)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](02-cj-data-visualization-salyers-cara_files/figure-markdown_github/Q1-1.png)

------------------------------------------------------------------------

**Question 2**: Create a line chart that plots total sales value over time. Is there anything unusual in the graph?
*This question grows your ability to use `geom_line()`.*

**ANSWER:** There are two days (around 280 and 640) that have really big dips in sales (sales = 0), and there is also a positive linear trend in the beginning that then seems to flatten out.

``` r
transaction_data %>%
  group_by(day) %>%
  summarize(total_sales = sum(sales_value, na.rm = TRUE)) %>%
  ggplot(aes(day, total_sales)) +
  geom_line()
```

![](02-cj-data-visualization-salyers-cara_files/figure-markdown_github/Q2-1.png)

------------------------------------------------------------------------

**Question 3**: Create a bar chart comparing total sales value of private label versus national brands. Assign different colors to the bars using the `fill` argument inside `aes()`.
*This question grows your ability to use `geom_bar()` along with its `stat` argument.*

**ANSWER:**

``` r
my_transaction_data <- left_join(transaction_data, product, by='product_id')

my_transaction_data %>%
  group_by(brand) %>%
  summarize(total_sales = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = brand, y = total_sales, fill = brand), stat='identity')
```

![](02-cj-data-visualization-salyers-cara_files/figure-markdown_github/Q3-1.png)

------------------------------------------------------------------------

**Question 4**: Building upon Question 3, we suspect customers prefer national brands for soft drinks, but less so for diary products like cheese. Confirm this by creating a stacked bar chart showing the split of cheese sales between national and private brands and a similar split for soft drinks.

Hint: Follow these steps to create your plot:

-   Use `my_transaction_data` to filter to only transactions with `commodity_desc` equal to "SOFT DRINKS" or "CHEESE"
-   Calculate the total sales value by `commodity_desc` and `brand`
-   Create the bars using `geom_bar` with `stat='identity'` and `position='fill'`

**ANSWER:**

``` r
my_transaction_data %>%
  filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(commodity_desc, brand) %>%
  summarize(total_sales = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = commodity_desc, y = total_sales, fill = brand), 
           stat = 'identity', position = "fill")
```

![](02-cj-data-visualization-salyers-cara_files/figure-markdown_github/Q4-1.png)

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

**ANSWER:** 18oz is the most popular size of peanut butter and jelly products

``` r
pb_and_j_data %>%
  ggplot() +
  geom_bar(aes(product_size))
```

![](02-cj-data-visualization-salyers-cara_files/figure-markdown_github/Q5.-1.png)