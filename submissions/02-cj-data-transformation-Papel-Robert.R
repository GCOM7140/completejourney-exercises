#' ---
#' title: "Answers to the 02 CJ Data Transformation Exercise"
#' author: Robert Papel
#' date: April 3, 2018
#' output: github_document
#' ---

knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(completejourney)
library(dplyr)

transaction_data #to see the table

#' **Question 1**: Create a histogram of quantity. What, if anything, do you
#' find unusual about this visualization?

ggplot(transaction_data) +
  geom_histogram(aes(x = quantity))

#' **Answer**: The histogram has a weird long and low bar, to the right of the
#' main column. I find this odd. And weird. 

#' **Question 2**: Use a line graph to plot total sales value by day. What, if
#' anything, do you find unusual about this visualization?

transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = day, y = total_sales_value))

#' **Answer**: The graph has some spikes, around day 250(ish), and same with
#' 630(ish). Also, the line graph itself may not be the most useful way to
#' visualize this data.

#' **Question 3**: Use a bar graph to compare the total sales values of 
#' national and private-label brands.
 
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')

my_transaction_data %>% 
  group_by(brand) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = FALSE)) %>%
  ggplot() +
  geom_bar(aes(brand, total_sales_value, fill = brand), stat = "identity")

#' **Question 4**: Building on Question 3, suppose you want to understand
#' whether the retailer's customers' preference for national brands (compared #' to private-label brands) is stronger in the soft drink category than it is #' in the cheese category. Examine this supposition by using a stacked bar 
#' graph to compare the split between national and private-label brands for  
#' soft drinks and cheeses.
 
my_transaction_data %>% 
  filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>% 
  group_by(commodity_desc, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot() +
  geom_bar(
    aes(commodity_desc, total_sales_value, fill = brand),
    stat = "identity",
    position = "fill"
  )
    
#' **Question 5**: The code below filters my_transaction_data to include only
#' peanut better, jelly, and jam transactions. Then it creates a new variable
#' named product_size equal to product size in ounces. Create a bar graph with
#' pb_and_j_data to visualize the distribution of the retailer's PB&J
#' transactions by product size. Which two product sizes are the most popular?

pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )  

ggplot(pb_and_j_data) +
  geom_bar(aes(product_size))

#' **Answer**: Product size 18 and 32 are the two most popular sizes!




