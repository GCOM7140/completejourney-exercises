library(tidyverse)
library(completejourney)

#Question 1
ggplot(data=transaction_data)+geom_histogram(mapping = aes(x = quantity))
#the histrogram looks like a single bar because it has such a long tail 

#Question 2
transaction_data %>%
  group_by(day) %>%
  mutate(total_sales = sum(sales_value, na.rm = TRUE)) %>%
  ggplot()+ geom_line(mapping = aes(x=day, y = total_sales))
#There were 2 days where there were no sales, most likely were holidays 

#Question 3 
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
my_transaction_data %>%
  group_by(brand) %>%
  summarize(total_sales = sum(sales_value)) %>%
  ggplot() + geom_bar(mapping = aes(x = brand, y = total_sales), 
    stat = 'identity')

#Question 4 
my_transaction_data %>%
  filter(commodity_desc %in% c ('SOFT DRINKS', 'CHEESE')) %>%
  group_by(commodity_desc, brand) %>%
  summarize(total_sales = sum(sales_value)) %>%
  ggplot()+geom_bar(mapping = aes(x = commodity_desc, y = total_sales, fill = brand),
                    stat = 'identity', position = 'fill')

#Question 5 
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )

ggplot(pb_and_j_data) + geom_bar(aes(x=product_size))

#18oz and 32oz
  