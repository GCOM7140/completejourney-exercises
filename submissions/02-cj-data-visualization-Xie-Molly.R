library(tidyverse)
library(completejourney)

# Question 1: Create a histogram of quantity. What, if anything, do you find unusual about this visualization?

ggplot(data = transaction_data) +
  geom_histogram(mapping = aes(x = quantity))

# Question 2: Use a line graph to plot total sales value by day. What, if anything, do you find unusual about this visualization?

transaction_data %>% 
  group_by(day) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(mapping = aes(x = day, y = total_sales_value))

#Question 3
my_transaction_data <- left_join(transaction_data, product, by = 'product_id')

my_transaction_data %>%
  group_by(brand) %>%
  summarize(sales = sum(sales_value)) %>%
  ggplot(aes(brand, sales)) +
  geom_bar(stat = "identity", fill = "lightblue", alpha = .8) +
  theme_bw() +
  scale_y_continuous(labels = comma)



# Question 4

my_transaction_data %>% 
  filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>% 
  group_by(commodity_desc, brand) %>% 
  summarize(total_sales = sum(sales_value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_bar(aes(commodity_desc, total_sales, fill = brand),
           stat = 'identity',
           position = 'fill')