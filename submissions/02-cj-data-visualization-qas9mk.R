library(tidyverse)
library(completejourney)
library(lubridate)

df <- data("transactions")
df1 <- data("products")

#Question 1 

#manipulating bins to get a proper look at the data
ggplot(transactions, aes(quantity)) + geom_histogram(bins = 50)
#the vast majority of the transactions are below a few thousand in quantity, but there is a large spike at around q = 10,000

#Question 2

#creating a date variable
transactions <- mutate(transactions, date = date(transaction_timestamp))

ggplot(transactions, aes(x = date, y = sales_value)) + geom_line()
#large spikes occur at seemingly unrelated times, such as in spring and early winter 2017

#Question 3

transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% 
  mutate(brand  = fct_explicit_na(brand)) %>% 
  filter(brand != "(Missing)")

ggplot(transactions_products, aes(x = brand, y = sales_value )) + geom_bar(stat = "identity")

#Question 4

transactions_products %>%
  filter(product_category %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(
    mapping  = aes(x = product_category, y = total_sales_value, fill = brand)
  ) + 
  geom_col(position = "fill")


#Question 5

pbj <- filter(transactions_products, product_category == "PNT BTR/JELLY/JAMS")
ggplot(pbj, aes(package_size)) + geom_bar()
#18 and 32 ounce are the most popular