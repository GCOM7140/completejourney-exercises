library(tidyverse)
library(completejourney)
library(lubridate)

#' Question 1: Explore the distribution of quantity. What, if anything, do you find unusual about this visualization?

ggplot(data=transactions) +
  geom_histogram(mapping = aes(x=quantity))
#' the way that the visualization is set up makes it seem as if most of the quantity is zero but this most likely has to do with the scale of the x variable 


#' Question 2: Use a line graph to plot total sales value by date. What, if anything, do you find unusual about this visualization?

transactions %>%
  mutate(date = date(transaction_timestamp)) %>%
  group_by(date) %>%
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(mapping = aes(x= date, y=total_sales_value))
#' there is a wide variety in the total sales value making the graph go up and down very frequently which in my opinion makes it hard to interpret what was happening on a specific day 

#' Question 3: Use a bar chart to compare the total sales value of national brands with that of private-label brands using the brand variable in the products dataset.

transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% 
  mutate(brand  = fct_explicit_na(brand)) %>% 
  filter(brand != "(Missing)")

transactions_products %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=brand, y=total_sales_value)) +
  geom_bar(stat = "identity")

#' Question 4: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar chart to compare the split between national and private-label brands for soft drinks and cheeses.

transactions_products %>%
  filter(product_category == "SOFT DRINKS" | product_category == "CHEESE") %>%
  group_by(brand, product_category) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(mapping = aes(x= product_category, y=total_sales_value, fill=brand)) +
  geom_col(position = "fill")

#' Question 5: Filter transactions_products for transactions in the peanut better, jelly, and jams product category (i.e., "PNT BTR/JELLY/JAMS"). Then, create a bar chart to visualize the distribution of the retailer's PB&J transactions by package size. Which two package sizes are the most popular?

transactions_products %>%
  filter(product_category == "PNT BTR/JELLY/JAMS") %>%
  group_by(package_size) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_bar(mapping=aes(x=package_size %>% fct_reorder(count), y=count), stat = "identity") +
  coord_flip()
#' the two most popular package sizes are 18oz and 32oz 
