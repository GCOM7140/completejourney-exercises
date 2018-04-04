#Customer Journey questions #2 


library(tidyverse)
library(completejourney)
library(ggplot2)
library(dplyr)


######### Question 1 

ggplot(data = transaction_data, mapping = aes(quantity)) +
  geom_histogram()
# this quantity x axis puts all the observations into one bar so no real insights can be gained 


############ Question 2 

transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales = sum(sales_value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(mapping = aes(x = day, y = total_sales)) 

# there seems to be too much information, I tihnk a larger time frame would allow for more use of this graph 

############# Question 3 

my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
my_transaction_data %>%
  group_by(brand) %>%
  mutate (total_sales_value_by_brand = sum (sales_value, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(mapping = aes( x = brand, y = total_sales_value_by_brand), stat = 'identity')

# this graph displays the overwhelming purchasing of national brands 

########### Question 4 

my_transaction_data %>%
  filter(commodity_desc %in% c('SOFT DRINKS', 'CHEESE')) %>%
  group_by(commodity_desc, brand) %>%
  summarise (total_sales_value_2 = sum (sales_value), na.rm = TRUE) %>%
  ggplot() + geom_bar (mapping = aes( x = commodity_desc, y = total_sales_value_2, fill = brand), stat = 'identity', position = 'fill') 



######### Question 4 

pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )      
ggplot(pb_and_j_data) + 
  geom_bar(aes(x = product_size))


# 18 and 32 are the most popular sizes 




