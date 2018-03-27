library(tidyverse)
library(nycflights13)

#Question 1.1 How many flights flew into LAX?

?flights
str(flights)
question_1.1 <- filter(flights, dest=="LAX")
nrow(question_1.1)
#Question 1.2 How many flights flew into LAX?
question_1.2 <- filter(flights, origin=="LAX")
nrow(question_1.2)

#Question 1.3 How many flights flew into LAX?
question_1.3 <- filter(flights, distance>=2000)
nrow(question_1.3)

#How many flights were destined for airports in the Los Angeles area (LAX, ONT, SNA, PSP, SBD, BUR, or LGB), but did not originate out of JFK?
destination<-c("LAX","ONT","SNA","PSP","SBD","BUR","LGB")
question_1.4 <- filter(flights, dest %in% destination & origin != "JFK")
nrow(question_1.4)

#Question 2.1
question_2.1<- filter(flights, !is.na(dep_time) & is.na(arr_time)) 
nrow(question_2.1)

#Question 3 
arrange(flights, desc(is.na(flights$arr_time)))


#Question 4
select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = FALSE))
#The default function is insensitive to case. This can be changed by using ignore.case function.

#Question 5
flights %>% filter(distance>=2000 & dep_delay >=0) %>% group_by(dest)%>%
  summarize(total_delay = sum(dep_delay,na.rm=TRUE))%>%
  mutate(proportions=total_delay/sum(total_delay))%>%
  arrange(desc(proportions))
library(tidyverse)
library(completejourney)

library(completejourney)
transaction_data <- transaction_data %>% mutate(retail_disc=abs(retail_disc)) %>% mutate(coupon_disc=abs(coupon_disc))  %>% mutate(coupon_match_dis=abs(coupon_match_disc))

#Question 2
transaction_data <-transaction_data %>% mutate(regular_price=(sales_value + retail_disc + coupon_match_disc) / quantity, 
                                               loyalty_price = (sales_value + coupon_match_disc) / quantity, 
                                               coupon_price = (sales_value - coupon_disc) / quantity)

#Question 3
regularpriceonedollar<- filter(transaction_data,regular_price <= 1)
n_distinct(regularpriceonedollar$product_id)
coupon<-filter(transaction_data,coupon_price<=1)
n_distinct(coupon$product_id)
loyalty<-filter(transaction_data,loyalty_price<=1)
n_distinct(loyalty$product_id)

#Question 4
transaction_data %>% group_by (basket_id) %>% filter(sales_value > 10)

#Question 5
transaction_data_new <-transaction_data %>% group_by(store_id) %>% summarize(total_sales =sum(sales_value)) %>% filter(total_sales>10000)

transaction_data  %>% filter(store_id %in% transaction_data_new$store_id) %>% mutate (pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>% group_by(store_id) %>% arrange(desc(pct_loyalty_disc))

str(transaction_data) 

