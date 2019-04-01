
#-------------------------------------#
# Theme: Data visualization (CJ）#
# Author: Roy Luo #
#------------------------------------#

library(tidyverse)
library(completejourney)
library(lubridate)

# Question 1: Explore the distribution of quantity. What, if anything, do you find unusual about this visualization?

ggplot(data = transactions) + 
  geom_histogram(mapping = aes(x = quantity))

# Can 'Mapping' be left out? #  
# The unsual part is that the variable quantity has a very long tail, which makes quantity = 0 has an extremely long bar. 
# Althernative#
ggplot(data = transactions %>% filter(quantity <= 10)) + 
  geom_histogram(mapping = aes(x = quantity))

# Question 2: Use a line graph to plot total sales value by date. What, if anything, do you find unusual about this visualization?

transactions %>% 
  mutate(date = date(transaction_timestamp)) %>% 
  group_by(date) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(aes(x = date, y = total_sales_value))
# The lowest value happened at late November (Black Friday) while the highest happended around late Decemeber (Christmas and New Year). 

# Question 3: Use a bar chart to compare the total sales value of national brands with that of private-label brands using the brand variable in the products dataset.

# Step 1: Join data #
transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% 
  mutate(brand  = fct_explicit_na(brand)) %>% 
  # fct_explicit_na:This gives missing value an explicit factor level, ensuring that they appear in summaries and on plots.
  filter(brand != "(Missing)")

transactions_products %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(mapping = aes(x = brand, y = total_sales_value)) + 
  geom_bar(stat = "identity")
# By default, geom_bar uses stat="bin" . This makes the height of each bar equal to the number of cases in each group, and it is incompatible with mapping values to the y aesthetic. If you want the heights of the bars to represent values in the data, use stat="identity" and map a value to the y aesthetic.

# Question 4:Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar chart to compare the split between national and private-label brands for soft drinks and cheeses.

# Stack bar #
transactions_products %>%
  filter(product_category %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(product_category, brand) %>% 
  # Can group multiple dimensions #
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(
    mapping  = aes(x = product_category, y = total_sales_value, fill = brand)
  ) + 
  geom_col(position = "fill")

# Question 5: Filter transactions_products for transactions in the peanut better, jelly, and jams product category (i.e., "PNT BTR/JELLY/JAMS"). Then, create a bar chart to visualize the distribution(count) of the retailer's PB&J transactions by package size. Which two package sizes are the most popular？
transactions_products %>%
  filter(product_category %in% "PNT BTR/JELLY/JAMS") %>%
  group_by(package_size) %>%
  summarise(count=n()) %>%
  ggplot()+
  geom_bar(
    aes(x = package_size %>% fct_reorder(count), y = count),
    stat    = "identity" #why necessary?#
  ) +
  coord_flip()

# The most popular package size is 18 oz.










