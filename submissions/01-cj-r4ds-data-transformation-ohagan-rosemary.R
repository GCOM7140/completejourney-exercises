####### Rosemary O'Hagan ###########
####### Customer Analytics HW #1 ###
####### 3/27/18 ####################

######################################################
##### R4DS Questions #################################
######################################################

library(tidyverse)
library(nycflights13)
?flights

##### Question 1 ###############################################################
# How many flights flew into LAX?

into_lax <- filter(flights, dest == "LAX") 
nrow(into_lax) #16,174

# How many flights flew out of LAX?

outLAX <- filter(flights, origin == "LAX")
nrow(outLAX) #0

# How many flights are greater than or equal to 2000 miles?

flights2000 <- filter(flights, distance >= 2000)
nrow(flights2000) #51,695

# How many flights destined for airports in LA area but not from JFK?

toLAnotfromNYC <- filter(flights, 
                         dest %in% c("LAX", "ONT", "SNA", "PSP", 
                                     "SBD", "BUR", "LGB") 
                         & !(origin == "JFK"))
nrow(toLAnotfromNYC) #5,737

##### Question 2 ###############################################################
# How many flights were "ghost flights"?

ghostflights <- filter(flights, is.na(arr_time))
nrow(ghostflights) #8,713

##### Question 3 ###############################################################
# How does arrange() treat missing values? How could you sort all the rows with 
# a missing arr_time to the top of the dataset?

# arrange() always sorts missing values at the end... 
arrange(flights, desc(is.na(arr_time)))

##### Question 4 ###############################################################
# What do you observe after running the code below? How does this behavior 
# reflect how select() helpers deal with uppercase and lowercase matching by 
# default? How can you change that default?

select(flights, contains("TIME"))
# select() is not case sensitive by default 
?select # change default setting of ignore.case from TRUE to FALSE
select(flights, contains("TIME", ignore.case = FALSE)) # no tibble

##### Question 4 ###############################################################
# For each destination greater than or equal to 2000 miles away, compute total 
# minutes of delay. Also determine what proportion of these total minutes of 
# delay the destinations represent. What are the top-three destinations by 
# proportion of total, and what proportions do each of the top-three 
# destinations represent?

### REDO!!!!!!!!!!!!!

######################################################
##### Customer Journey Questions #####################
######################################################

library(tidyverse)
library(completejourney)

transaction_data <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time
  )

##### Question 1 ###############################################################
# Change the discount variables (i.e., retail_disc, coupon_disc, 
# coupon_match_disc) from negative to positive.

transaction_data_q1 <- transaction_data %>% 
  mutate(retail_disc = abs(retail_disc),
         coupon_disc = abs(coupon_disc),
         coupon_match_disc = abs(coupon_match_disc)) 
transaction_data_q1

##### Question 2 ####################################################
# Create three new variables named regular_price, loyalty_price, and 
# coupon_price according to the following logic:
##### regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity
##### loyalty_price = (sales_value + coupon_match_disc) / quantity
##### coupon_price  = (sales_value - coupon_disc) / quantity

transaction_data_q2 <- transaction_data_q1 %>% 
  mutate(regular_price = 
           (sales_value + retail_disc + coupon_match_disc) / quantity,
         loyalty_price = 
           (sales_value + coupon_match_disc) / quantity,
         coupon_price  = 
           (sales_value - coupon_disc) / quantity)
transaction_data_q2

##### Question 3 ###############################################################
# transaction_data includes 92,339 unique product IDs. How many of these 
# products (not transactions!) had a regular price of one dollar or less? What 
# does this count equal for loyalty and coupon prices?

transaction_data_q3a <- filter(transaction_data, regular_price <= 1)  
n_distinct(select(transaction_dataQ3a,product_id)) #12,442

transaction_data_q3b <- filter(transaction_data, loyalty_price <= 1)  
n_distinct(select(transaction_dataQ3b,product_id)) #20,113

transaction_data_q3c <- filter(transaction_data, coupon_price <= 1)  
n_distinct(select(transaction_dataQ3c,product_id)) #22,273

##### Question 4 ####################################################
# What proportion of baskets are over $10 in sales value?
### FINISH LATER!!!!!!
