# Jenny Zhao
# 02-data-visualization

#knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
devtools::install_github('GCOM7140/completejourney', 
                         auth_token = '06f39fa251cff106a40a34f0c8edb6d1c818b5ba')
library(completejourney)

transaction_data

# Question 1
ggplot(data = transaction_data) + geom_histogram(mapping = aes(x = quantity))

# This graph only has one bar visually distinguishable. Probably, the other quantities have much smaller count.


# Question 2
transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = day, y = total_sales_value))

# This line graph vibrate up and down a lot. In general, total sales value stays around 10000 to 15000 after 100 days.


# Question 3
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')

my_transaction_data %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot() + 
  geom_bar(
    mapping = aes(x = brand, y = total_sales_value, fill = brand), 
    stat = 'identity'
  )

# Question 4
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

# retailer's customers' preference for national brands is stronger in the soft drink category.

# Question 5
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )

ggplot(pb_and_j_data) + geom_bar(aes(x = product_size))

# product size 18 and 32 are the top 2 most popluar size
