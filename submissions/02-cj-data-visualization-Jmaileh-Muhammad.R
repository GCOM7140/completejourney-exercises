#' --- 
#' title: "Data Visualization Exercises for Complete Journey" 
#' author: Mo Jmaileh
#' date: April 3, 2018 
#' ---

knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(completejourney)

# Question 1: Create a histogram of quantity. What, if anything, do you find unusual about this visualization?

ggplot(transaction_data, aes(x = quantity)) + geom_histogram()

#This plot has one really high value near 1 or 2 items, and very small amounts after that. Doesn't effectively convey much meaning other than that customers usually buy less than 5 items. For some reason the tail is really long as well.

#Question 2: Use a line graph to plot total sales value by day. What, if anything, do you find unusual about this visualization?
  
transaction_data %>%
  group_by(day) %>%
  summarise(total_sales = sum(sales_value, na.rm = TRUE)) %>%
  ggplot(aes(day, total_sales)) +
  geom_line()

#The graph has a ramp up in the beginning, which is unusual. This could be bringing people onboard to the program. Furthermore, there are some days at zero, which could mean closures for holidays or bad weather.

#Question 3: Use a bar graph to compare the total sales values of national and private-label brands.
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')

my_transaction_data %>%
  group_by(brand) %>%
  summarise(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot(aes(brand, total_sales_value)) +
  geom_bar(stat = 'identity')


#Question 4: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar graph to compare the split between national and private-label brands for soft drinks and cheeses.

my_transaction_data %>%
  filter(commodity_desc %in% c("CHEESE", "SOFT DRINKS")) %>%
  group_by(brand, commodity_desc) %>%
  summarise(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot(aes(commodity_desc, total_sales_value, fill = brand)) +
  geom_bar(stat = 'identity', position = 'fill')

#Question 5: The code below filters my_transaction_data to include only peanut better, jelly, and jam transactions. Then it creates a new variable named product_size equal to product size in ounces. Create a bar graph with pb_and_j_data to visualize the distribution of the retailer's PB&J transactions by product size. Which two product sizes are the most popular?

pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))

ggplot(pb_and_j_data) + 
  geom_bar(aes(x = product_size))
