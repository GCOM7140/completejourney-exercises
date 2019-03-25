---
title: "Data Visualization Solutions"
author: Sebastian Cook 
---
  
  
library(tidyverse)
library(completejourney)
library(lubridate)

---
  
  ** Question 1**: Explore the distribution of `quantity`. What, if anything, do
you find unusual about this visualization?
  

```{r include = TRUE, eval = TRUE, message = FALSE}
ggplot(data = transactions) + 
  geom_histogram(mapping = aes(x = quantity))

#  This histogram has an extremely long tail, with just a single bar with a high value at 0 Quantity. This skew in the data may be eliminated if we filter the data.

ggplot(data = transactions %>% filter(quantity <= 10)) + 
  geom_histogram(mapping = aes(x = quantity))
```

---
  
  **Question 2**: Use a line graph to plot total sales value by date. What, if
anything, do you find unusual about this visualization?
  

transactions %>% 
  mutate(date = date(transaction_timestamp)) %>% 
  group_by(date) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = date, y = total_sales_value))

# This data has a high degree of autocorrelation, with the data seeming to alternate between rather large peaks and valleys.

```

---
  
  **Question 3**: Use a bar chart to compare the total sales value of national
brands with that of private-label brands using the `brand` variable in the
`products` dataset.

Because `transactions` does not contain product metadata, you will need to
create a new dataset with product information in it. Consider running the code
below for this purpose and using `transactions_products` for your answer.

```{r eval = TRUE}
transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% 
  mutate(brand  = fct_explicit_na(brand)) %>% 
  filter(brand != "(Missing)")
```


```{r include = TRUE, eval = TRUE}
transactions_products %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(mapping = aes(brand, total_sales_value)) + 
  geom_bar(stat = "identity")
```

---
  
  **Question 4**: Building on Question 3, suppose you want to understand whether
the retailer's customers' preference for national brands (compared to
                                                          private-label brands) is stronger in the soft drink category than it is in the
cheese category. Examine this supposition by using a stacked bar chart to
compare the split between national and private-label brands for soft drinks and
cheeses.

**Hint**: Follow these three steps to create your plot: 
  
  - Filter `transactions_products` to include only transactions with
`product_category` equal to "SOFT DRINKS" or "CHEESE"
- Calculate total sales value by `product_category` and `brand`
- Create the bars using [`geom_col()`][arnold 3.7.2] with `position = 'fill'`

```{r include = TRUE, eval = TRUE}
transactions_products %>%
  filter(product_category %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(
    mapping  = aes(product_category, total_sales_value, fill = brand)
  ) + 
  geom_col(position = "fill")
```

---
  
  **Question 5**: Filter `transactions_products` for transactions in the peanut
better, jelly, and jams product category (i.e., `"PNT BTR/JELLY/JAMS"`). Then,
create a bar chart to visualize the distribution of the retailer's PB\&J
transactions by package size. Which two package sizes are the most popular?

```{r include = TRUE, eval = TRUE}
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

# 18OZ PB and J packages are the most popular, followed by the larger, 32 ounce containers.
```