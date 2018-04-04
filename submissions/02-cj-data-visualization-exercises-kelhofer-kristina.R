#' ---
#' title: Complete Journey Data Visualization Exercises
#' author: Kristina Kelhofer
#' date: April 3, 2018
#' ouptut: github_document
#' ---
#' ================
#' 
library(tidyverse)
library(completejourney)
??completejourney
#' Question 1
ggplot(data = transaction_data) + geom_histogram(aes(quantity))

ggplot(data = transaction_data) + geom_histogram(mapping = aes(quantity), bins = 10) + xlim(c(0,30))

# This histogram is unusual because of the automatic scale, meaning there is likely an outlier. To take a clearer look, I limited the x-scale.

#' Question 2
transaction_data %>%
group_by(day) %>%
  summarize(day_sales = sum(sales_value)) %>%
  ggplot() + 
  geom_line(aes(day, day_sales))

#The extreme highs and lows seen in the sales value are unusual. I also find the trend unusual, and would want to dig deeper into why the sales per day increased in that pattern.

#' Question 3
my_transaction_data <- left_join(transaction_data,product, by = 'product_id')

my_transaction_data %>%
  group_by(brand) %>%
  summarize(brand_sales = sum(sales_value)) %>%
  ggplot() + 
  geom_bar(aes(brand, brand_sales), stat = 'identity')

#' Question 4

my_transaction_data %>%
  filter(commodity_desc == c("SOFT DRINKS", 
                            "CHEESE")) %>%
  group_by(brand, commodity_desc) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot() + geom_bar(aes(x = commodity_desc, 
                          y = total_sales_value, fill = brand), stat = "identity", position = "fill")

#' Question 5 
pb_and_j_data <- my_transaction_data %>% 
  filter(commodity_desc == 'PNT BTR/JELLY/JAMS') %>%
  mutate(
    product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                             curr_size_of_product)))
  )
\ 
pb_and_j_data %>%
  ggplot() + geom_bar(aes(product_size))

# 18 oz is the most popular size, followed by 32 oz