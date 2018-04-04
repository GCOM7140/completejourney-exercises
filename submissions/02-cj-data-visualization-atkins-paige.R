#1
   library(tidyverse)
   library(completejourney)
   ggplot(data = transaction_data) + geom_histogram(mapping = aes(x = quantity))
   # this histogram graph has a heavy concentration of data at 0. Only zooming in allows my eyes to see there is more than just that one bar. 
   
#2
   transaction_data %>% 
     group_by(day) %>% 
     summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
     ggplot() + 
     geom_line(mapping = aes(x = day, y = total_sales_value))
    #The only strange thing I see in this graph is the quick drop around the 300th and 650th day in the data
   
#3  
   my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
 
   my_transaction_data %>%
     group_by(brand) %>%
     summarize(total_sales_value = sum(sales_value)) %>%
     ggplot() + 
     geom_bar(
       mapping = aes(x = brand, y = total_sales_value), 
       stat = 'identity'
     )   
   
#4
   my_transaction_data %>%
     filter(commodity_desc %in% c('SOFT DRINKS', 'CHEESE')) %>%
     group_by(commodity_desc, brand) %>%
     summarize(total_sales_value = sum(sales_value)) %>%
     ggplot() + geom_bar(
     mapping  = aes(x = commodity_desc, y = total_sales_value, fill = brand), 
     stat     = 'identity', 
    position = 'fill')
   
#5
   #pb_and_j_data <- my_transaction_data %>% 
     #filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
     #mutate( product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
     #curr_size_of_product))))
   
   ggplot(pb_and_j_data) + 
     geom_bar(aes(x = product_size))