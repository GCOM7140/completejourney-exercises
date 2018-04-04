#Rich Eldh
#Professor Boichuk
#Customer Analytics
#Due Date: 3 April 2018


#-----------------------------------------------------------------------------
#Assignment 2 - Complete Journey Exercise
#-----------------------------------------------------------------------------

library(tidyverse)
library(completejourney)
dat.transaction <- transaction_data
dat.transaction
product

#-----------------------------------------------------------------------------
#Question 1
#-----------------------------------------------------------------------------

ggplot(data = dat.transaction, mapping = aes(quantity)) +
  geom_histogram()

#The range of values for quantity leaves all entries listed in a single bar column. Most purchases involved one to several products, so the range of quantity should be much smaller to accomdate this. 

#-----------------------------------------------------------------------------
#Question 2
#-----------------------------------------------------------------------------

total.sales <- dat.transaction %>% 
  group_by(day) %>% 
  summarize(total_sales = sum(sales_value))

ggplot(data = total.sales, aes(x = day, y = total_sales)) +
  geom_line()

#Too many x-values are on the graph, making it bunched together and difficult to read. It would likely be better to measure total sales by week, or even month, for a cleaner graph. A trend line may also be more appropriate than a line measuring exact values by day, week or month. 

#-----------------------------------------------------------------------------
#Question 3
#-----------------------------------------------------------------------------

my_transaction_data <- left_join(dat.transaction, product, by = 'product_id')

my_transaction_data %>% 
  group_by(brand) %>% 
  summarize(total_sales = sum(sales_value)) %>% 
  ggplot(mapping = aes(x = brand, y = total_sales)) +
    geom_bar(stat = 'identity')


#-----------------------------------------------------------------------------
#Question 4
#-----------------------------------------------------------------------------

my_transaction_data %>% 
  filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>% 
  group_by(commodity_desc, brand) %>% 
  summarize(total_sales = sum(sales_value)) %>% 
  ggplot(mapping = aes(x = commodity_desc, y = total_sales, fill = brand)) +
    geom_bar(stat = 'identity', position = 'fill')

#-----------------------------------------------------------------------------
#Question 5
#-----------------------------------------------------------------------------

pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )


ggplot(data = pb_and_j_data) +
  geom_bar(mapping = aes(x = product_size))
#The first most popular size is 18, followed by 32.

