#' ---
#' title: "CJ Exercise #2"
#' author: Dominic Sfreddo
#' date: March 30, 2018
#' output: github_document
#' ---


library(tidyverse)
library(completejourney)


#1

transaction_data %>% 
  filter(quantity <= 7) %>% 
ggplot() +
  geom_histogram(
    aes(x = quantity),
    binwidth = 1
  )
#Had to filter since it was giving records where quantity went to the tens of thousands.

#2

transaction_data %>%
  group_by(day) %>% 
  summarise(sales_per_day = sum(sales_value)) %>% 
  ggplot() +
  geom_line(
    aes(x = day, y = sales_per_day)
  )
#The plot jumps and spikes randomly at random times and there is a lot of variation. Possible missing data for days where sales value is zero.

#3


product_trans_data <- left_join(transaction_data, product, by = "product_id")

ggplot(data = product_trans_data, aes(brand)) +
  geom_bar(
    aes(weight = sales_value, fill = brand)
  )
?aes
?geom_bar

#4

my_transaction_data <- product_trans_data %>% 
  filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE"))
my_transaction_data %>% 
  group_by(commodity_desc, brand) %>% 
  summarise(total_sales_value = sum(sales_value)) %>% 
  ggplot() +
    geom_bar(stat = 'identity', position = 'fill',
      aes(x = commodity_desc, y = total_sales_value, fill = brand)
    )

ggplot(data = my_transaction_data, aes(commodity_desc)) +
  geom_bar(
    aes(weight = sales_value, fill = brand)
  )
  
#5

my_transaction_data <- left_join(transaction_data, product, by = 'product_id')
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  select(curr_size_of_product) %>%
  mutate(product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)',
                                                  '\\1', curr_size_of_product))))

pb_and_j_data %>% 
  ggplot() +
  geom_bar(
    aes(x = product_size, fill = product_size)
  )
