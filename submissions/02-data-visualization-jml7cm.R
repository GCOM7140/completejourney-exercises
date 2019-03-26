#' title: "Complete Journey Data Visualization Exercises"
#' author: J. McLean Long
#' output: github-document

# Below is the code for the second set of Complete Journey Exercises 

# Make sure to load all the necessary libraries for whatever study or exercises you are working on in

library(devtools)

library(tidyverse)

library(completejourney)

library(lubridate)

### Question 1 ### - this question concerns 'quantity' and how it affects the 'Complete Journey' and data in R as a whole. 

ggplot(data = transactions) + 
  geom_histogram(mapping = aes(x = quantity))

# I found this output strange and the solutions confirmed one oddity of this output is the really long tail. It looks like a normal bar graph plot until you see the long base which is an interesting visualization. 

### Question 2 ### - I need to comment on the best day of the year and any outliers. 

transactions %>% 
  mutate(date = date(transaction_timestamp)) %>% 
  group_by(date) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = date, y = total_sales_value))

# This result looks like the Richter Scale from an earthquake. There were good and bad and sales days, but there was a massive spike right before the X axis gets to "Jan 2018". This makes sense to me since sales often peak during the Holidays and the tallest spike in sales data fits that normal conclusion based on past experience.

### Question 3 ###

transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% 
  mutate(brand  = fct_explicit_na(brand)) %>% 
  filter(brand != "(Missing)")

transactions_products %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(mapping = aes(x = brand, y = total_sales_value)) + 
  geom_bar(stat = "identity")

# This results in an interesting plot between National and Private. This is one of the things I like about R - it creates a nice clean visualization. 

### Question 4 ### - this question builds on the code from Question 3.

transactions_products %>%
  filter(product_category %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(
    mapping  = aes(x = product_category, y = total_sales_value, fill = brand)
  ) + 
  geom_col(position = "fill")

# This fills in some of the missing data from Q3. It breaks down the data into Cheese and Soft Drinks and makes the visualization even better from a viewer perspective.

### Question 5 ###

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

# I really like this visualization. It took me a while to get the code correct, but it results in the spread you might see from one side of a pop. pyramid and I think it gives a really unique insight into this 'transactions' data that the other visualizations do not. 

### End of CJ code until Exercise 3 ###


