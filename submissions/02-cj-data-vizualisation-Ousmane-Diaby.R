library(completejourney)

#question 1

ggplot(data = transaction_data) + geom_histogram(mapping = aes(x = quantity))

# one bar is just exremely long and does not seem right

#question 2

linegraph <- transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE))

  ggplot(data=linegraph) + 
  geom_line(mapping = aes(x = day, y = total_sales_value))
#the sale value from day 0 to 100 are relatevily smaller than the rest of the time. there are close to zero total sales on some days, and double the average amount on some other days.

#question 3
  my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
  my_transaction_data %>%
    group_by(brand) %>%
    summarize(total_sales_value = sum(sales_value)) %>%
    ggplot() + 
    geom_bar(
      mapping = aes(x = brand, y = total_sales_value), 
      stat = 'identity'
    )
?geom_bar  

#question 4

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
#question 5
  pb_and_j_data <- my_transaction_data %>% 
    filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
    mutate(
      product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                               curr_size_of_product)))
    )
  ggplot(pb_and_j_data) + 
    geom_bar(aes(x = product_size))

  # product size 18 ounces, and 32 next.