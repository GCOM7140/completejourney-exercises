# Reid Calhoun
# Customer Journey HW #2 -- Data Visualization
# Due March 3, 2018


library(tidyverse)
library(completejourney)



##################
### Question 1 ###
##################
  
  
# Create a histogram of quantity. What, if anything, do you find unusual about 
# this visualization?

ggplot(data = transaction_data) +
  geom_histogram(mapping = aes(x = quantity))

# This histogram is accumulating every single item bought during the test period
# Need to filter further so that R knows to split quantity by a certain 
# characteristic





##################
### Question 2 ###
##################

# Use a line graph to plot total sales value by day. What, if anything, 
# do you find unusual about this visualization?

transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales_value_by_day = sum(sales_value)) %>% 
  ggplot(aes(day,total_sales_value_by_day)) +
  geom_line()
  
# There is a ton of noise for one - the data changes massively by day. 
# Secondly, total sales largely increases the first 100 days and then stabilizes






##################
### Question 3 ###
##################



# Use a bar graph to compare the total sales values of national and 
# private-label brands.

#Hint: Because transaction_data does not contain product metadata, run the code below to create a new dataset with additional product information in it. Use my_transaction_data for your answer.


my_transaction_data <- left_join(transaction_data, product, by = 'product_id')


my_transaction_data %>% 
  group_by(brand) %>% 
  summarize(sales_by_brand = sum(sales_value)) %>% 
  ggplot(aes(brand, sales_by_brand)) +
  geom_bar(stat = "identity")
  
  
  



##################
### Question 4 ###
##################
  

# Building on Question 3, suppose you want to understand whether the retailer's 
# customers' preference for national brands (compared to private-label brands) 
# is stronger in the soft drink category than it is in the cheese category.
# Examine this supposition by using a stacked bar graph to compare the split
# between national and private-label brands for soft drinks and cheeses.

my_transaction_data  %>% 
  filter(commodity_desc == "SOFT DRINKS" | commodity_desc == "CHEESE") %>% 
  group_by(commodity_desc, brand) %>% 
  summarize(total_sales = sum(sales_value)) %>% 
  ggplot(aes(commodity_desc, total_sales, fill = brand)) +
  geom_bar(
    stat = "identity",
    position = "fill"
    )
  
# Customers much prefer the national brand for soft drinks  and are almost split
# for the national vs. private brand for cheese. Customers seem to slightly 
# prefer the private brnad for cheese.






##################
### Question 5 ###
##################


#The code below filters my_transaction_data to include only peanut better, jelly, and jam transactions. 
#Then it creates a new variable named product_size equal to product size in ounces



pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
mutate(
product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
curr_size_of_product)))
)


#Create a bar graph with pb_and_j_data to visualize the distribution of the retailer's PB&J transactions by product size. Which two product sizes are the most popular?


ggplot(data = pb_and_j_data) +
  geom_bar(aes(x = product_size))

# The most popular product size is 18 ounces, followed by 32 ounces

