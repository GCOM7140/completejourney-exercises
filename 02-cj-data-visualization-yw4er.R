library(tidyverse)
library(completejourney)
library(lubridate) # see chapter 16 of r4ds

listtable
transaction

#  Q1
ggplot(data = transactions) + 
  geom_histogram(mapping = aes(x = quantity))

# In this histograms, one of the bar is so long that other bars are almost disappear. Also then quantity actually only distributed between 0 to a very small number(10), so the Xcaxis is unapproprate now. This problem can be modified by add a "filter" at quantity.

ggplot(data = transactions %>% filter(quantity <= 10)) + 
  geom_histogram(mapping = aes(x = quantity))

# Question 2: Use a line graph to plot total sales value by date. What, if anything, do you find unusual about this visualization?

#geom_line()

transactions %>% 
  mutate(date = date(transaction_timestamp)) %>% 
  group_by(date) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = date, y = total_sales_value))

#You can get dates from the transaction_timestamp variable in the transactions dataset with the date() function of the lubridate package. If you are interested, you can see some examples of other accessor functions in the lubridate package here.

#Question 3: Use a bar chart to compare the total sales value of national brands with that of private-label brands using the brand variable in the products dataset.

# 先把两个table合起来
transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% # 先把两个table合起来
  mutate(brand  = fct_explicit_na(brand)) %>% 
  filter(brand != "(Missing)")
#geom_bar()
transactions_products %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(mapping = aes(x = brand, y = total_sales_value)) + 
  geom_bar(stat = "identity")


#Question 4: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar chart to compare the split between national and private-label brands for soft drinks and cheeses.

#Hint: 
#Filter transactions_products to include only transactions with product_category equal to "SOFT DRINKS" or "CHEESE"
#Calculate total sales value by product_category and brand
#Create the bars using geom_col() with position = 'fill'

transactions_products %>%
  filter(product_category %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(
    mapping  = aes(x = product_category, y = total_sales_value, fill = brand)
  ) + 
  geom_col(position = "fill") # 用这个来分position

#Question 5: Filter transactions_products for transactions in the peanut better, jelly, and jams product category (i.e., "PNT BTR/JELLY/JAMS"). Then, create a bar chart to visualize the distribution of the retailer's PB&J transactions by package size. Which two package sizes are the most popular? # 18 oz and 32oz

transactions_products %>% 
  filter(product_category == "PNT BTR/JELLY/JAMS") %>% 
  group_by(package_size) %>% 
  summarize(count = n()) %>% 
  ggplot() + 
  geom_bar(
    mapping = aes(x = package_size %>% fct_reorder(count), y = count), 
    stat    = "identity"
  ) +
  coord_flip()
