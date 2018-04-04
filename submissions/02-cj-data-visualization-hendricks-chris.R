---
  title: "Data Visualization Submission - Chris Hendricks"
output: github_document
---
  
  #Trying to do the r markdown file thing ^
  
#Start of prep for questions
  
  library(tidyverse)
library(completejourney)

#Let's go over the data real quick

str(transaction_data)

###

##**Question 1**: Create a histogram of `quantity`. What, if anything, do you find unusual about this visualization? 
  
##This question is designed to strengthen your ability to use `geom_histogram()`.

ggplot(transaction_data, aes(quantity)) + 
  geom_histogram()

#It looks like the data is all stacked into one section. Not sure if there is some sort of interal error with ggplot causing this but its clear there's something fishy going on.

###

#**Question 2**: Use a line graph to plot total sales value by day. What, if anything, do you find unusual about this visualization?
  
#This question is designed to strengthen your ability to use `dplyr` verbs in combination with `geom_line()`.

transaction_data %>% 
group_by(day) %>% 
summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
ggplot() + 
geom_line(mapping = aes(x = day, y = total_sales_value))

#Hmmm. There are some very large spikes throughout the data that are unusual and it is worth investigating why sales dipped/jumped during those time periods. Other than these spikes it seems pretty "normal". The growth at the beginning of the line probably represents the start of data collection / early growth in participants in the dataset (since we are looking at total sales value).

###

#**Question 3**: Use a bar graph to compare the total sales values of national and private-label brands. 

#**Hint**: Because `transaction_data` does not contain product metadata, run the code below to create a new dataset with additional product information in it. Use `my_transaction_data` for your answer.

#review data
str(transaction_data)
#first create the new table with Boichuk code
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
#review new data
str(my_transaction_data)
#From here we can see the stuff we need, like the brand column which shows private/national

#Now we create our code for the bar graph
my_transaction_data %>%
#We want to see results by brand type
group_by(brand) %>%
#calculate the total sales value
summarize(total_sales_value = sum(sales_value)) %>%
#and visualize
ggplot() + 
geom_bar(mapping = aes(x = brand, y = total_sales_value), stat = 'identity')

###

##**Question 4**: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar graph to compare the split between national and private-label brands for soft drinks and cheeses. 

#**Hint**: Follow these three steps to create your plot: 
  
#  - Filter `my_transaction_data` to include only transactions with `commodity_desc` equal to "SOFT DRINKS" or "CHEESE" 
#- Calculate total sales value by `commodity_desc` and `brand`
#- Create the bars using `geom_bar` with `stat = 'identity'` and `position = 'fill'`

#Step 1, the filter
my_transaction_data %>%
filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>%
#Step 2, caluclating and grouping
group_by(commodity_desc, brand) %>%
summarize(total_sales_value = sum(sales_value)) %>%
#Step 3, creating the bar chart
ggplot() + 
geom_bar(mapping  = aes(x = commodity_desc, y = total_sales_value, fill = brand), stat = 'identity', position = 'fill')

#Now let's interpret. We can clearly see that there is greater sales value in national brand sodas vs private label, while in regards to cheese the split is about even. However, I don't think this is enough to conclude that consumer preference for national brands is stronger in the soft drinks category. It could be that private label cheese is just really darn good. Or the opposite could be true and private label soda could just be fairly crappy, on average. Could also be a matter of there being many private labels of cheese all competing with the big brands, and there being few private label sodas that can jostle with the soda giants like Pepsi and Coca-cola (for lots of reasons), and this lowers their sales volume. Lots more analysis is needed.

###

#**Question 5**: The code below filters `my_transaction_data` to include only peanut better, jelly, and jam transactions. Then it creates a new variable named `product_size` equal to product size in ounces. Create a bar graph with `pb_and_j_data` to visualize the distribution of the retailer's PB\&J transactions by product size. Which two product sizes are the most popular?

#Loading question code...
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )

#Bar chart code...
ggplot(pb_and_j_data, aes(x = product_size)) + 
geom_bar()

#Wow, clearly the 18oz jars are the most popular, with a far and away second of 32oz jars. I wonder what it is about the 18oz that is so preferable over other sizes?

#End of assignment 2

