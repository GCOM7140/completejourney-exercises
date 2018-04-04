#' HW 2  George Dowling

library(tidyverse)
library(completejourney)

####' Question 1 ####
ggplot(data = transaction_data)+
  geom_histogram(aes(quantity)) 
 

#' The scale is poor and there is a large right tail which makes its hard to understand 


####' Question 2 ####
  transaction_data %>%
  group_by(day) %>%
  summarise(agg_sales = sum(sales_value, na.rm = T)) %>%
  ggplot()+
  geom_line(aes(x = day, y = agg_sales), color = "magenta")


####' Question 3 ####
transaction_product <- left_join(transaction_data, product, by = "product_id")

transaction_product %>%
  group_by(brand) %>%
  summarise(agg_sales = sum(sales_value)) %>%
  ggplot(aes(brand, agg_sales, fill = brand))+
         geom_bar(stat = "identity")


####' Question 4 ####
transaction_product %>%
  filter(commodity_desc %in% c("SOFT DRINKS", "CHEESE")) %>%
  group_by(commodity_desc, brand) %>%
  summarise(agg_sales = sum(sales_value)) %>%
  ggplot(aes(commodity_desc, agg_sales, fill = brand))+
        geom_bar(stat = "identity", position = "fill")


####' Question 5 ####

transaction_product %>%
  filter(commodity_desc == "PNT BTR/JELLY/JAMS") %>%
  mutate(product_size = as.factor(as.integer(gsub('([0-9]+)([[:space:]]*OZ)', '\\1',
                                                  curr_size_of_product)))) %>%
  ggplot(aes(product_size)) +
           geom_bar()

#' 18oz and 32oz



