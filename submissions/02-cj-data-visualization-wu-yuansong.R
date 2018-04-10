#Data Visualization Exercise - completejourney
#Yuansong(Kevin) Wu

library(tidyverse)
library(completejourney)

#Q1
ggplot(data = transaction_data) +
  geom_histogram(mapping = aes( x = quantity))
#There is only one bar.

#Q2
transaction_data %>%
  group_by (day) %>%
  mutate (total_sales_value = sum (sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes( x = day, y = total_sales_value))

#Q3
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
my_transaction_data %>%
  group_by(brand) %>%
  mutate (total_sales_value_by_brand = sum (sales_value, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(mapping = aes( x = brand, y = total_sales_value_by_brand), stat = 'identity')

#Q4
#Filter my_transaction_data to include only transactions with commodity_desc equal to "SOFT DRINKS" or "CHEESE"
my_transaction_data %>%
  filter(commodity_desc %in% c('SOFT DRINKS', 'CHEESE')) %>%
  #Calculate total sales value by commodity_desc and brand
  group_by(commodity_desc, brand) %>%
  summarise (total_sales_value_2 = sum (sales_value), na.rm = TRUE) %>%
  #Create the bars using geom_bar with stat = 'identity' and position = 'fill'
  ggplot() +
  geom_bar (mapping = aes( x = commodity_desc, y = total_sales_value_2, fill = brand), stat = 'identity', position = 'fill') 

#Q5
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )      
ggplot(pb_and_j_data) + 
  geom_bar(aes(x = product_size))