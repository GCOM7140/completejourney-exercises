# Alex Ledoux

##  question 1
# install.packages("devtools")
devtools::install_github("bradleyboehmke/completejourney")
library(tidyverse)
library(completejourney)
library(lubridate) # see chapter 16 of r4ds

ggplot(data = transactions) + 
  geom_histogram(mapping = aes(x = quantity))

# The unusual aspect of the histogram is its extremely long tail. The distance
# is so far that the histogram almost appears to be a single bar at zero. This
# distortion warrants additional steps (e.g., `filter(quantity <= 10)`).

ggplot(data = transactions %>% filter(quantity <= 10)) + 
  geom_histogram(mapping = aes(x = quantity))

# question 2 ##

transactions %>% 
  mutate(date = date(transaction_timestamp)) %>% 
  group_by(date) %>% 
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = date, y = total_sales_value))

# The most extreme days of the year were November 23 and December 23,
# respectively. Sales on November 23 were likely very low due to it being Black
# Friday, and sales on December 23 were likely very high due to it being
# Christmas Eve.

# question 3 ##

transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% 
  mutate(brand  = fct_explicit_na(brand)) %>% 
  filter(brand != "(Missing)")
transactions_products %>%
  group_by(brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(mapping = aes(x = brand, y = total_sales_value)) + 
  geom_bar(stat = "identity")


## Question 4 ##

transactions_products %>%
  filter(product_category %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value)) %>%
  ggplot(
    mapping  = aes(x = product_category, y = total_sales_value, fill = brand)
  ) + 
  geom_col(position = "fill")


# question 5 ##

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

# The most popular package size for PB&J products is 18 oz. The runner-up is 32
# oz.