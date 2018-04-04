#' ---
#' title: "Answers to the complete journey data visulization Exercise"
#' author: Zoe Zheng
#' date: April 3, 2018
#' output: github_document
#' ---
  
library(tidyverse)
library(completejourney)
library(ggplot2)

#' **Question 1**: Create a histogram of quantity. Is there anything unusual in the graph?
ggplot(transaction_data,aes(quantity)) + geom_histogram()
#'
#' The problem for this graph is that there are lots of records that have quantity of 0, so the graph is extremely concentrated on 0.

#' **Question 2**: Create a line chart that plots total sales value over time. Is there anything unusual in the graph?
#' 
transaction_data %>% 
  group_by(day) %>% 
  summarise(total_sales=sum(sales_value)) %>% 
  ggplot() + geom_line(aes(day,total_sales))

#' The difference of total sales between 0-100 day and 100-700 day is huge. And there are several days that has 0 total sales value.


#' **Question 3**:  Create a bar chart comparing total sales value of private label versus national brands. Assign different colors to the bars using the `fill` argument inside `aes()`.

new_transaction_data <- left_join(transaction_data, product, by='product_id')

new_transaction_data%>% 
  group_by(brand) %>% 
  summarise(total_sales=sum(sales_value)) %>% 
  ggplot() + geom_bar(mapping = aes(x = brand,y = total_sales,fill = brand),stat = 'identity')
 

#' **Question 4**: Building upon Question 3, we suspect customers prefer national brands for soft drinks, but less so for diary products like cheese. Confirm this by creating a stacked bar chart showing the split of cheese sales between national and private brands and a similar split for soft drinks.

t_data<-new_transaction_data %>% 
  filter(commodity_desc %in% c('SOFT DRINKS','CHEESE')) %>% 
  group_by(commodity_desc,brand) %>% 
  summarise(total_sales=sum(sales_value)) 

ggplot(t_data) + geom_bar(mapping = aes(x = commodity_desc, y = total_sales, fill = brand), stat = 'identity', position = 'fill')


#' **Question 5**: Below is a block of code that creates a dataset of transactions of peanut better, jelly and jams with the product size determined in ounces. Use the `pb_and_j_data` dataset to create a bar plot that shows the most popular size (in ounces) of peanut butter and jelly products.


my_transaction_data <- left_join(transaction_data, product, by='product_id')
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  select(curr_size_of_product) %>%
  mutate(product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)','\\1', curr_size_of_product))))


ggplot(pb_and_j_data) + geom_bar(mapping = aes(product_size))
