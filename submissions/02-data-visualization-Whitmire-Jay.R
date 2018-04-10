##### Jay Whitmire

library(tidyverse)
library(completejourney)

#Question 1: Create a histogram of quantity. What, if anything, do you find unusual about this visualization?
completejourney::transaction_data
transaction_data %>%
  ggplot() +
  geom_histogram(aes(x = quantity))

# there are a lot of 0 quantities which doesn't make sense

#Question 2: Use a line graph to plot total sales value by day. What, if anything, do you find unusual about this visualization?

transaction_data %>%
  ggplot() +
  geom_line(aes(x = day, y = sales_value))

# sales value only goes a little past 800 per day, I would have thought this would be higher. The spikes seem to be weekly

# Question 3: Use a bar graph to compare the total sales values of national and private-label brands.

my_transaction_data <- left_join(transaction_data, product, by = 'product_id')

my_transaction_data %>%
  ggplot() +
  geom_bar(mapping = aes(x = brand, y = sales_value), stat = "identity")

#Question 4: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar graph to compare the split between national and private-label brands for soft drinks and cheeses.

#my attempt, took me a while, didn;t figure it out
filter(my_transaction_data, commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>%
  ggplot() +
  geom_bar(mapping = aes(x = commodity_desc, y = total_sales_value, fill = "brand" ), stat = "identity", position = "fill")

head(my_transaction_data)

# the answer
my_transaction_data %>%
  filter(commodity_desc %in% c('SOFT DRINKS', 'CHEESE')) %>%
  group_by(commodity_desc, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot() + 
  geom_bar(
    mapping  = aes(x = commodity_desc, y = total_sales_value, fill = brand), 
    stat     = 'identity', 
    position = 'fill'
  )

# Question 5: The code below filters my_transaction_data to include only peanut better, jelly, and jam transactions. Then it creates a new variable named product_size equal to product size in ounces. Create a bar graph with pb_and_j_data to visualize the distribution of the retailer's PB&J transactions by product size. Which two product sizes are the most popular?

pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )


pb_and_j_data %>%
  ggplot() + 
  geom_bar(aes(x = product_size))

# 18 and 32 ounces are the most popular