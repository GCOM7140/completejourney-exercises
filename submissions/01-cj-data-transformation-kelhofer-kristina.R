library(tidyverse)
#install.packages("nycflights13")
library(nycflights13)
?flights

flights <- flights

#R4DS Flight Questions
#question 1
#How many flights flew into LAX?
flights %>%
  filter(dest == "LAX") %>%
  select(dest) %>%
  nrow()
  #16174
#√

#How many flights flew out of LAX?
flights %>%
  filter(origin == "LAX") %>%
  select(dest) %>%
  nrow()
  #0
#√

#How many flights are greater than or equal to 2000 miles?
flights %>%
  filter(distance >= 2000) %>%
  nrow()
  #51695
#√

#How many flights were destined for airports in the Los Angeles area (LAX, ONT,
  #SNA, PSP, SBD, BUR, or LGB), but did not originate out of JFK?
flights %>%
  filter(dest %in% c("LAX","ONT","SNA", "PSP", "SBD","BUR", "LGB"),
         origin != "JFK") %>%
  select(dest) %>%
  nrow()
  #5737


#question 2
  flights %>%
    filter(is.na(dep_time) == FALSE, is.na(arr_time) == TRUE) %>%
    select(dep_time, arr_time) %>%
    nrow()  
  #458  

#question 3
  flights %>%
    arrange(desc(is.na(arr_time)), desc(arr_time)) %>%
    select(arr_time)
  #got stuck and googled!
  
#question 4
  select(flights, contains("TIME"))
  #This selects all variables with the text "time" in their name. Select is not 
  #case sensitive, nor does it need time to come at any specific point in the 
  #variable (beginning or end). To change the default setting of Select, you could add a modifier after "TIME", setting ignore.case to FALSE. ##Got stuck and ultimately found one solution on stackoverflow
  
#Question 5
  flights %>% 
    filter(distance >= 2000, dep_delay > 0) %>%
    group_by(dest) %>%
    summarize(Tdelay = sum(dep_delay)) %>%
    mutate(prop_delay = Tdelay/ sum(Tdelay)) %>%
    arrange(desc(prop_delay)) %>% 
    head(3)

  #SFO, LAX, LAS
  
#Complete Journey Data Questions
  library(completejourney)
  transaction_data <- transaction_data %>%
    select(
      quantity,
      sales_value,
      retail_disc,coupon_disc,coupon_match_disc,
      household_key,store_id,basket_id,product_id,
      week_no, day, trans_time
    )

  #question 1
  (transaction_data <- transaction_data %>%
    mutate(retail_disc       = abs(retail_disc), 
           coupon_disc       = abs(coupon_disc), 
           coupon_match_disc = abs(coupon_match_disc)))

  #question 2
  (transaction_data <- transaction_data %>%
  mutate(regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
         loyalty_price = (sales_value + coupon_match_disc) / quantity,
         coupon_price  = (sales_value - coupon_disc) / quantity))

  #question 3
  transaction_data %>%
    filter(regular_price <= 1) %>%
    select(product_id) %>%
    n_distinct()
#12442
  transaction_data %>%
    filter(loyalty_price <= 1) %>%
    select(product_id) %>%
    n_distinct()
  #20113
  transaction_data %>%
    filter(coupon_price <= 1) %>%
    select(product_id) %>%
    n_distinct()
  #22273
  
  #question 4
  transaction_data %>%
    group_by(basket_id) %>%
    summarize(basket_sales = sum(sales_value)) %>%
    mutate(proportion = mean(basket_sales > 10)) %>%
    ungroup(basket_id)
  # 0.654
  
  #question 5
  transaction_data <- transaction_data %>%
    mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price))
    
 transaction_data %>%  
   filter(
     is.finite(regular_price),
     is.finite(loyalty_price),
     regular_price > 0
   ) %>%
    group_by(store_id) %>%
   summarize(
     total_sales_value    = sum(sales_value),
     avg_pct_loyalty_disc = mean(pct_loyalty_disc)
   ) %>%
    filter(total_sales_value > 10000) %>%
    arrange(avg_pct_loyalty_disc)
  
  
  #had to consult solutions to get number 5! was missing the first filter and summarizing loyalty disc
 #got 317, but I didn't do it in the right order
  
  