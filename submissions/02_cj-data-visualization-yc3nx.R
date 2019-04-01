library(tidyverse)
library(completejourney)
library(ggplot2)
library(lubridate) # see chapter 16 of r4ds

# Question 1: Explore the distribution of quantity. What, if anything, do you find unusual about this visualization?
  
  ggplot(transactions, aes(quantity)) + 
    geom_histogram(bins=25)
# most have very low quantities. 
  transactions %>%
    filter(quantity < 15) %>%
    ggplot(aes(quantity)) + 
    geom_histogram(bins=45)+
    scale_x_continuous( breaks= seq(0,15, 2.5) )
  
# Question 2: Use a line graph to plot total sales value by date. What, if anything, do you find unusual about this visualization?
  
transactions$date<- date(transactions$transaction_timestamp)

transactions %>%
  group_by(date) %>%
  summarize(total_sale_value = sum(sales_value,na.rm=T))%>%
  ggplot(aes(date, total_sale_value))+
  geom_line(size=0.7)
# The highest sales are during Christmas. 


# Question 3: Use a bar chart to compare the total sales value of national brands with that of private-label brands using the brand variable in the products dataset.

# Because transactions does not contain product metadata, you will need to create a new dataset with product information in it. Consider running the code below for this purpose and using transactions_products for your answer.

transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% 
  mutate(brand  = fct_explicit_na(brand)) %>% 
  filter(brand != "(Missing)")

transactions_products %>%
  group_by(brand) %>% 
  summarize(total_sales_value = sum(sales_value,na.rm=T)) %>%
  ggplot(aes(brand,total_sales_value))+
  geom_bar(stat = "identity")

# Question 4: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar chart to compare the split between national and private-label brands for soft drinks and cheeses.

  
  # Filter transactions_products to include only transactions with product_category equal to "SOFT DRINKS" or "CHEESE"
transactions_products %>%
  filter(product_category %in% c('SOFT DRINKS','CHEESE'))%>%
# Calculate total sales value by product_category and brand
  group_by(product_category, brand) %>%
  summarize(total_sales_value = sum(sales_value))%>%
  # Create the bars using geom_col() with position = 'fill'
  ggplot(aes(product_category,total_sales_value,fill=brand))+
  geom_col()
# soft drinks has a much higher sales, and the proportion of natioanl brand is much larger.

# Question 5: Filter transactions_products for transactions in the peanut better, jelly, and jams product category (i.e., "PNT BTR/JELLY/JAMS"). Then, create a bar chart to visualize the distribution of the retailer's PB&J transactions by package size. Which two package sizes are the most popular?

transactions_products %>%
  filter(product_category == 'PNT BTR/JELLY/JAMS') %>%
  ggplot(aes(package_size))+
  geom_bar()
# 18 oz and 32 oz. 












