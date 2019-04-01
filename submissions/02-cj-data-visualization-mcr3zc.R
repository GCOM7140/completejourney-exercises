install.packages(c("completejourney", "lubridate")) 
library(tidyverse)
library(completejourney)
library(lubridate)

#Q1 Explore the distribution of quantity. What, if anything, do you find unusual about this visualization?

ggplot(data = transactions) + 
  geom_histogram(mapping = aes(x = quantity))

#there is only one very long and very large coordinate so you can only see a long outlier in the visualization and there is no distribution to report on. 
# The scale of coordinate is too long which makes the bar chart hard to identify

#Q2 Use a line graph to plot total sales value by date. What, if anything, do you find unusual about this visualization?

transactions %>% 
  mutate(date = date(transaction_timestamp)) %>% 
  group_by(date) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = date, y = total_sales_value))

#There are two extreme sales value on November 23 and December 23. The value on November 3 is a low outlier and the value on December 23 is a high outlier. 

#Q3  Use a bar chart to compare the total sales value of national brands with that of private-label brands using the brand variable in the products dataset.


transactions_products %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(mapping = aes(x = brand, y = total_sales_value)) + 
  geom_bar(stat = "identity")

#This bar chart says that national brands make up a larger proportion of total sales value than private label brands. 

#Q4 Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar chart to compare the split between national and private-label brands for soft drinks and cheeses.

transactions_products %>%
  filter(product_category %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(
    mapping  = aes(x = product_category, y = total_sales_value, fill = brand)
  ) + 
  geom_col(position = "fill")

#This bar chart shows us that retail customers have a strong preference for national brands over private label brands in the field of soft drinks, but in the field of chesse, they skew more towards the private label brands (which make up more than 50% of the total sales value) as opposed to the national ones. 


# Question 5 --> Filter transactions_products for transactions in the peanut better, jelly, and jams product category (i.e., "PNT BTR/JELLY/JAMS"). Then, create a bar chart to visualize the distribution of the retailer's PB&J transactions by package size. Which two package sizes are the most popular?

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

#The most popular package sizes are the 18 OZ and the 32 OZ. The 18 OZ is much more popular by an order of magnitude of almost triple, than the 32 OZ. 