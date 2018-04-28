#Michael Starnes
#GCOM 7140
#Professor Boichuck

library(tidyverse)
library(completejourney)


#Question 1: Historgram
  histogram  <-ggplot(data = transaction_data) 
  histogram + geom_histogram(mapping = aes(x = quantity))
    #the histogram is strange because the distribution is heavily weighted into the first bar, and doesn't really show the second bar's information that well.
  
  
#Question 2:
  transaction_data %>% 
    group_by(day) %>% 
    summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
    ggplot() + 
    geom_line(mapping = aes(x=day, y=total_sales_value))
    #the two really big drops were shocking, as was the increase going up to the 200th day of the observations, which then seem to stall for the period  

#Question 3: Bar graph comparison
  my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
    
    
    my_transaction_data %>%
      group_by(brand) %>%
      summarize(total_sales_value = sum(sales_value)) %>%
      ggplot() + 
      geom_bar(
         mapping = aes(x=brand, y=total_sales_value), 
        stat = 'identity'
      )
  #national brands are showing up as the clear majority for sales value
    
    
#Question 4:Stacked Bar
    
    my_transaction_data %>%
      filter(commodity_desc %in% c('SOFT DRINKS', 'CHEESE')) %>%
      group_by(commodity_desc, brand) %>%
      summarize(total_sales_value = sum(sales_value)) %>%
      ggplot() + 
      
      
      geom_bar(
        mapping=aes(x = commodity_desc, y = total_sales_value, fill = brand), 
        stat='identity', 
        position='fill'
      )
    
        #Soft Drinks have a much larger share of national brand sales value.
    
#Question 5: PB&J Bar Graph
    pb_and_j_data <- my_transaction_data %>% 
      filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
      
      mutate(
        product_size = as.factor(as.integer(gsub('([0:9]+)([[:space:]]*OZ)', '\\1',
                                                 curr_size_of_product)))
      )
    
    
    ggplot(pb_and_j_data) +
    geom_bar(aes(x= product_size))
    #most of the product seems to be sold in 18 ounce containers, with 32 ounces being the second most common
    