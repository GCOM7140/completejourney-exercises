library(tidyverse)
library(completejourney)
library(lubridate) 
get_data(which = "transactions", verbose = FALSE)
get_data(which = "products", verbose = FALSE)

#Question 1: Explore the distribution of quantity. What, if anything, do you find unusual about this visualization?
ggplot(data = transactions) + 
  geom_histogram(mapping = aes(x = quantity))
#concentration of values around 0
ggplot(data = filter(transactions, quantity <= 10)) + 
  geom_histogram(mapping = aes(x = quantity))

#Question 2: Use a line graph to plot total sales value by date. What, if anything, do you find unusual about this visualization?
transactions %>% 
  mutate(date = date(transaction_timestamp)) %>% 
  group_by(date) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = date, y = total_sales_value))

#Question 3: Use a bar chart to compare the total sales value of national brands with that of private-label brands using the brand variable in the products dataset.
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
  ggplot() +
  geom_bar(mapping = aes(x = brand, y = total_sales_value), stat = "identity")

#Question 4: suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. 
transactions_products <- filter(transactions_products, product_category %in% c("SOFT DRINKS", "CHEESE"))
transactions_products %>%
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(
    mapping  = aes(x = product_category, y = total_sales_value, fill = brand)
  ) + 
  geom_col(position = "fill")

#Question 5: Filter transactions_products for transactions in the peanut better, jelly, and jams product category (i.e., "PNT BTR/JELLY/JAMS"). Then, create a bar chart to visualize the distribution of the retailer's PB&J transactions by package size. Which two package sizes are the most popular?
transactions_products <- filter(transactions_products, product_category %in% c("PNT BTR","JELLY","JAMS"))
transactions_products %>% 
  group_by(package_size) %>% 
  summarize(count = n()) %>% 
  ggplot() + 
  geom_bar(
    mapping = aes(x = package_size %>% fct_reorder(count), y = count), 
    stat    = "identity"
  ) +
  coord_flip()