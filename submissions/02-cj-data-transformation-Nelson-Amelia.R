#' --- 
#' title: "complete journey exercise 2" 
#' author: Amelia Nelson
#' date: April 3, 2018 
#' output: github_document 
#' ---

library(tidyverse)
library(completejourney)

# Question 1: Create a histogram of quantity. What, if anything, do you find unusual about this visualization?

ggplot(data = transaction_data) +
  geom_histogram(mapping = aes(x = quantity))

# The histogram is extremely skewed left; virtually all of the items in the grocery store are bought in small quanitities per basket. 

# Question 2: Use a line graph to plot total sales value by day. What, if anything, do you find unusual about this visualization?

transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(mapping = aes(x = day, y = total_sales_value))

# The line graph shoes a fairly consistent range of sales values for most days besides a few irregularities. For the first 100 days, sales values are lower and increasing linearly, ramping up to the normal range of values. Secondly, there are two days in which the store had 0 sales. 

# Question 3: Use a bar graph to compare the total sales values of national and private-label brands.

my_transaction_data <- left_join(transaction_data, product, by = 'product_id')

my_transaction_data %>% 
  group_by(brand) %>% 
  summarize(total_sales_value = sum(sales_value)) %>% 
  ggplot() +
  geom_bar(
    mapping = aes(x = brand, y = total_sales_value),
    stat = 'identity'
  )

# National brands account for more than twice as many sales as Private brands. 

# Question 4: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar graph to compare the split between national and private-label brands for soft drinks and cheeses.

my_transaction_data %>% 
  filter(commodity_desc %in% c('SOFT DRINKS', 'CHEESE')) %>% 
  group_by(commodity_desc, brand) %>% 
  summarize(total_sales_value = sum(sales_value)) %>% 
  ggplot() +
  geom_bar(
    mapping = aes(x = commodity_desc, y = total_sales_value, fill = brand),
    stat = 'identity',
    position = 'fill'
  )

# Cheese has a much more even distribution of sales between national private. About 55% of sales are private cheese brands, and 45% of sales for national cheese brands. This distribution is much more uneven when it comes to soft drinks: Private brands account for only 10% of sales, while national brands account for the remaining 90%. 

# Question 5: The code below filters my_transaction_data to include only peanut better, jelly, and jam transactions. Then it creates a new variable named product_size equal to product size in ounces. Create a bar graph with pb_and_j_data to visualize the distribution of the retailer's PB&J transactions by product size. Which two product sizes are the most popular?

pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )

ggplot(pb_and_j_data) +
  geom_bar(aes(x = product_size))

# The most popular pb&j product size is 18 oz, and the second most popular size is 32 oz. 
  