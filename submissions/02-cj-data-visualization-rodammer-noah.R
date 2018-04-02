#############
# FILE INFO #
#############

# Title: GCOMM 7140 Homework 2
# Author: Noah Rodammer


#################
# LOAD PACKAGES #
#################

library(tidyverse)
library(dplyr)
library(devtools)
library(completejourney)


#organize columns systematically
transaction_data <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time
  )

######################
#  COMPLETE JOURNEY  #
#      EXERCISES     #
######################


#################
#  QUESTION #1  #
#################

#Create a history of quantity. Is there anything unusual in the graph?
#this question grows your ability to use geom_histogram()


transaction_data %>%
ggplot(aes(quantity)) +
  geom_histogram()

##There are some baskets that have an incredibly high quantity count; perhaps
##this is a data integrity error (may also be a bulk order, but unlikely) given
##the size

#################
#  QUESTION #2  #
#################

#Create a line chart that plots total sales value over time. Is there anything 
#unusual in the graph?

#this question grows to use geom_line()

transaction_data %>%
  group_by(day) %>%
  summarize(totalSale = sum(sales_value)) %>%
  ggplot(aes(x=day,y=totalSale)) +
  geom_line()

## The unusual part of this graph would be the fact that the sales volume 
## increased at first and then stabilized, as well as acted cyclically. The 
## drop to 0 is also odd, but maybe could be caused by Christmas or something. 

#################
#  QUESTION #3  #
#################

#Create a bar chart comparing total sales value of private label versus national 
#brands. Assign different colors to the bars using the fill argument inside 
#aes()

my_transaction_data <- left_join(transaction_data, product, by='product_id')

#This question grows your ability to use geom_bar() along with its stat 
#argument.

my_transaction_data %>%
  group_by(brand) %>%
  summarize(totalSale = sum(sales_value)) %>%
  ggplot() +
  geom_bar(mapping = aes(x=brand,y=totalSale,fill=brand),stat='identity')

  
#################
#  QUESTION #4  #
#################

#Building upon Question 3, we supsect customers prefer national brands for soft 
#drinks, but less so for diary products like cheese. Confirm this by creating a
#stacked bar chart showing the split of cheese sales between national and
#private brands and a simliar split for soft drinks.

#Hint: Follow these steps to create your plot:
##Use my_transaction_data to filter to only transactions with commodity_desc 
##equal to "SOFT DRINKS" or "CHEESE"
##Calculate the total sales value by commodity_desc and brand
##Create the bars using geom_bar with stat='identity' and position='fill'

my_transaction_data %>%
  filter(commodity_desc%in%c("SOFT DRINKS" ,"CHEESE")) %>%
  group_by(commodity_desc,brand) %>%
  summarize(totalSale = sum(sales_value)) %>%
  ggplot() +
  geom_bar(mapping = aes(x=commodity_desc,y=totalSale,fill=brand),stat='identity',position='fill')

##confirmed

#################
#  QUESTION #5  #
#################

#Below is a block of code that creates a dataset of transactions of peanut 
#better, jelly and jams with the product size determined in ounces. Use the 
#pb_and_j_data dataset to create a bar plot that shows the most popular size 
#(in ounces) of peanut butter and jelly products.

pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  select(curr_size_of_product) %>%
  mutate(product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)',
                                                  '\\1', curr_size_of_product))))

###

pb_and_j_data %>%
  ggplot()+
  geom_bar(aes(x=product_size))

##18 oz is largest
