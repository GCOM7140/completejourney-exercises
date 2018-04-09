library(tidyverse)
library(completejourney)

#Question 1: Create a histogram of quantity. What, if anything, do you find unusual about this visualization?

ggplot(data = transaction_data) + 
  geom_histogram(mapping = aes(x = quantity))

  #This graph appears to just be a single bar at quantity 0. 

#Question 2: Use a line graph to plot total sales value by day. What, if anything, do you find unusual about this visualization?

transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = day, y = total_sales_value))

  #Sales value appears to have a few dramatic spikes and drops despite remaining relatively consistent for the rest of the 2 year period. I'm not sure if the data factors out store closure, which would otherwise account for the drops to near-zero.

#Question 3: Use a bar graph to compare the total sales values of national and private-label brands.

my_transaction_data <- left_join(transaction_data, product, by = 'product_id')

my_transaction_data %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot() + 
  geom_bar(
    mapping = aes(x = brand, y = total_sales_value), 
    stat = 'identity'
  )

  #National brands are solidly outperforming private.

#Question 4: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar graph to compare the split between national and private-label brands for soft drinks and cheeses.

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

  #Cheese has a more even split between private and national, whereas soft drinks have a strong preference towards national

#Question 5: The code below filters my_transaction_data to include only peanut better, jelly, and jam transactions. Then it creates a new variable named product_size equal to product size in ounces. Create a bar graph with pb_and_j_data to visualize the distribution of the retailer's PB&J transactions by product size. Which two product sizes are the most popular?

ggplot(pb_and_j_data) + 
  geom_bar(aes(x = product_size))

  #18 & 32
