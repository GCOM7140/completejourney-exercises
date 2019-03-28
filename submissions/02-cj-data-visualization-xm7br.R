####################
#data visualization#
####################

library(tidyverse)
library(completejourney)
library(lubridate)

#Question 1: Explore the distribution of quantity. What, if anything, do you find unusual about this visualization?
ggplot(data = transactions) +
  geom_histogram(mapping = aes(x = quantity))

#this graph has a really long tail. To combat this, use filter

ggplot(data = transactions %>% filter(quantity <= 20)) + 
  geom_histogram(mapping = aes(x = quantity))

#Question 2: Use a line graph to plot total sales value by date. What, if anything, do you find unusual about this visualization?

sales_by_date <-
  transactions %>%
  mutate(date = date(transaction_timestamp)) %>%
  group_by(date) %>%
  summarize(total_sales = sum(sales_value)) 

ggplot(data = sales_by_date) +
  geom_line(mapping = aes(x = date, y = total_sales))

#We see the highest peak around Christmas time and lowest sales around the end of November. 

#Question 3: Use a bar chart to compare the total sales value of national brands with that of private-label brands using the brand variable in the products dataset.

#create a new object with data merged
product_sales <-
  left_join(
    transactions,
    products,
    by ="product_id"
  ) %>%
  mutate(brand = fct_explicit_na(brand, na_level = "NA")) %>%
  group_by(brand) %>%
  filter(brand != "NA")

#sum sales value by brand and plot
product_sales %>%
  summarize(total_sales = sum(sales_value)) %>%
  ggplot()+
  geom_bar(mapping = aes(x = brand, y = total_sales), stat = "identity")


#Question 4: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar chart to compare the split between national and private-label brands for soft drinks and cheeses.  
product_sales %>%
  filter(product_category %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(product_category, brand) %>%
  summarize(total_sales = sum(sales_value))%>%
  ggplot(
    mapping = aes(x = product_category, y = total_sales, fill = brand))+
  geom_col(position = "fill")

#Question 5: Filter transactions_products for transactions in the peanut better, jelly, and jams product category (i.e., "PNT BTR/JELLY/JAMS"). Then, create a bar chart to visualize the distribution of the retailer's PB&J transactions by package size. Which two package sizes are the most popular?

product_sales %>%
  mutate (package_size = fct_explicit_na(package_size, na_level = "NA")) %>%
  filter(product_category == "PNT BTR/JELLY/JAMS") %>%
  filter(package_size != "NA") %>%
  group_by(package_size) %>%
  summarize(count = n()) %>%
  ggplot()+
  geom_bar(mapping = aes(x = package_size, y = count), stat = "identity")
