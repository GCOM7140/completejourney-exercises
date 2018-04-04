# Lucas Pulliza
# Homework Set 2, CJ

library(tidyverse)
library(completejourney)
library(ggplot2)

# (1) Histogram of Quantity 
ggplot(data = transaction_data) + 
  geom_histogram(mapping = aes(x = quantity))
# Looks like a bar graph because of the tail

# (2) Line graph to plot total sales value by day
transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = day, y = total_sales_value))
# Rapid increase over first 100, perahps because the store just opened? And two zeros between 200/400 and after 600 likely due to store closures

# (3) Bar graph comparing national and private-label
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
my_transaction_data %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot() + 
  geom_bar(
    mapping = aes(x = brand, y = total_sales_value), 
    stat = 'identity'
  )

# (4) Soft drink and cheest national brands
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

# (5)
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )

ggplot(pb_and_j_data) + 
  geom_bar(aes(x = product_size))

pb_and_j_data %>% 
  count(product_size) %>% 
  arrange(-n)