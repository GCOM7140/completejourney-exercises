library(tidyverse)
library(completejourney)
library(lubridate)

#Question 1: Explore the distribution of quantity. What, if anything, do you find unusual about this visualization?
ggplot(data= transactions)+
  geom_histogram(mapping= aes(quantity))

ggplot(data= transactions %>% filter(quantity>5))+
  geom_histogram(mapping= aes(quantity))

# As most observations have 1 or 2 counts in quantity and few of observations have larger numbers of counts in quantity, on the graph,there is a single tall bar located around 0 and a long but thin tail extended to quantity more than 75000.


#Question 2: Use a line graph to plot total sales value by date. What, if anything, do you find unusual about this visualization?
transactions %>%
  mutate(date= date(transaction_timestamp)) %>%
  group_by(date)%>%
 summarise(total_sales= sum(sales_value))%>%
  #arrange(desc(total_sales))
  ggplot() +
  geom_line(mapping = aes(x= date,y=total_sales))

# The daily sales values stay relatively constant in the range from 10000 to 20000. But in Dec.23rd, the sales value was extremely high of 24760; the second highest sales date was Nov.22nd with 20145 in sales. 


#Question 3: Use a bar chart to compare the total sales value of national brands with that of private-label brands using the brand variable in the products dataset.
transactions_products <- left_join(
  transactions, 
  products, 
  by = "product_id"
) %>% 
  mutate(brand  = fct_explicit_na(brand)) %>% 
  filter(brand != "(Missing)")
  
  transactions_products %>%
  group_by(brand)%>%
  summarise(total_brand_sales = sum(sales_value)) %>%
  ggplot()+
  geom_bar(mapping = aes(x=brand,y= total_brand_sales), stat = "identity")
# National brands have a much higher sales value comparing to private brands.
  

#Question 4: Building on Question 3, suppose you want to understand whether the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category. Examine this supposition by using a stacked bar chart to compare the split between national and private-label brands for soft drinks and cheeses.
  
transactions_products %>%
  filter(product_category %in% c("SOFT DRINKS","CHEESE"))%>%
  group_by(product_category,brand)%>%
  summarise(total_sales_by_category_and_brand= sum(sales_value)) %>%
  ggplot()+
  geom_col(mapping = aes(x=brand,y=total_sales_by_category_and_brand, fill=product_category),position = "fill")
# Thus, the retailer's customers' preference for national brands (compared to private-label brands) is stronger in the soft drink category than it is in the cheese category.


#Question 5: Filter transactions_products for transactions in the peanut better, jelly, and jams product category (i.e., "PNT BTR/JELLY/JAMS"). Then, create a bar chart to visualize the distribution of the retailer's PB&J transactions by package size. Which two package sizes are the most popular?
transactions_products %>%
  filter(product_category == "PNT BTR/JELLY/JAMS")%>%
  group_by(package_size)%>%
  summarise(count_by_packagesize= n()) %>%
  #arrange(desc(count_by_packagesize))
  ggplot()+
  geom_col(mapping = aes(x=package_size %>% fct_reorder(count_by_packagesize)
                         ,y=count_by_packagesize))

# 18oz is most popular with count og 2905 and 32 oz is the second popular one with count of 737 




