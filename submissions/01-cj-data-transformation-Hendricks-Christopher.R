#how to submit

#go to sumbmissions folder in r for data science section

#follow naming conventions for submissions

#due tonight at 11:59 pm

#first, go to the repo in github

#got to clone or download, make sure ssh is on if used key

#go to r stuido, new project, version control, git

#paste in copied url, press tab, make sure the download is an intentional place

#open new session, create new project

#whatever is in the cloud will now be on local computer

#now what u have to do, put your file within this submissions folder

#youll see the submissions folder in the global environment

#mke sure to follow naming conventions

#first, pull the version to get most up to date

#then click commit, write commit message (e.g. "submitting hmwrk 1)

#hit commit, then click push

#should be done at this point

###########################################
###Start of Hmwrk #1###
###########################################

#Thank you Professor Boichuk for telling us about pipes in R.

#Worked alongside solutions on alot of this because I'm bad at R

#Prep the Data

setwd("C:/Users/hendr/Desktop/CustomerAnalyticsRDirectory")

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

###
###Start of Section A###
###

####More Data Prep

install.packages('nycflights13')

library(tidyverse)
library(nycflights13)

?flights

################
###Question 1###
################

#How many flights flew into LAX?
str(flights)

##Now we can see all the stuff we'll need for Q1 here, like the column names##


#Use filter to search data for instances of dest == LAX, pipe it to an nrow function.
filter(flights, dest =='LAX') %>%
nrow()
#16174

#How many flights flew out of LAX?
#To solve, use filter, same as above
filter(flights, origin == 'LAX') %>% 
nrow()
#0

#How many flights are greater than or equal to 2000 miles?

#Double check data
str(flights)

#Filter same as above
filter(flights, distance >= 2000) %>% 
nrow()
#51,695

# How many flights were destined for airports in the Los Angeles area (LAX, ONT, 
# SNA, PSP, SBD, BUR, or LGB), but did not originate out of JFK?


#Let's make a variable for our destinations for easy recall
Q1dest <-c("LAX", "ONT", "SNA", "PSP", "SBD", "BUR", "LGB")

#Run a filter pipeline to make sure our variable works
filter(flights, dest %in% Q1dest) %>%
nrow()

#Now to answer the question in code

filter(flights, dest %in% Q1dest, origin != 'JFK') %>%
nrow()
#5,737

################
###Question 2###
################

#How many flights were "ghost flights"? A "ghost flight" is defined as a flight that departed, but never arrived (i.e., has a missing arr_time).

#Look at data
str(flights)

#need arr_time, dep_time

filter(flights, dep_time > 0, arr_time == (is.na(arr_time))) %>%
nrow()
#The answer is 0
#would be easier if I could type arr_time == NULL but I think got it.

################
###Question 3###
################
#How does arrange() treat missing values? How could you sort all rows with a missing arr_time to the top of the dataset?

?arrange

arrange(flights, desc(is.na(arr_time)))
#Arrange takes an argument and displays rows according to the argument. It treats missing values by clumping them together with the NA label. Above code puts the NA arr_time rows at the top


################
###Question 4###
################

#What do you observe after running the code below? How does this behavior reflect how select() helpers deal with uppercase and lowercase matching by default? How can you change that default?
?contains

select(flights, contains("TIME"))

#This works perfectly. Contrary to the solution posted on github, contains is not case sensitive by default.

select(flights, contains("TIME", ignore.case = TRUE))
#Tested to make sure, this gives the same results as above. However, being aware of case sensitivty when using functions to analyze text is very important.


################
###Question 5###
################

#For each destination greater than or equal to 2000 miles away, compute total minutes of departure delay. Then determine what proportion of total-departure-delay minutes each destination represents. What three destinations top the list?

str(flights)

#check the data

#run the filter
filter(flights, dep_delay >= 0) %>%
#group to keep destinations
group_by(dest) %>%
#summarize for the caluclation
summarize(dep_delay_mins = sum(dep_delay)) %>%
#use mutate to figure out proportions
mutate(dep_delay_proportion = dep_delay_mins / sum(dep_delay_mins)) %>%
arrange(-dep_delay_proportion) %>%
#Without the -dep_delay_proportion, this doesn't sort properly, not sure why
head(3)

#End of Section 1#

########################
###Start of Section B###
########################

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

#Just redoing this part#

################
###Question 1###
################
#Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
#Hint: Use the abs() function within mutate().

#Pipe the data into mutate function, use abs to find absolute value
transaction_data %>% 
mutate(retail_disc = abs(retail_disc), coupon_disc = abs(coupon_disc),coupon_match_disc = abs(coupon_match_disc))

#Lets make sure that worked
filter(transaction_data, coupon_disc < 0) %>%
nrow()
#Seems to work fine!

################
###Question 2###
################
#Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:


transaction_data %>% 
#Long mutate inc. In class help basically solved this
mutate(
regular_price     = (sales_value + retail_disc + coupon_match_disc) / quantity,
loyalty_price     = (sales_value + coupon_match_disc) / quantity,
coupon_price      = (sales_value - coupon_disc) / quantity) %>% 
#View the rows
select(regular_price, loyalty_price, coupon_price, everything())

################
###Question 3###
################
#transaction_data includes 92,339 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty and coupon prices?

transaction_data %>% 
  filter(regular_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()

transaction_data %>% 
  filter(loyalty_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()

transaction_data %>% 
  filter(coupon_price <= 1) %>% 
  select(product_id) %>% 
  n_distinct()

################
###Question 4###
################
#What proportion of baskets are over $10 in sales value?

transaction_data %>%
  group_by(basket_id) %>%
  summarize(basket_value = sum(sales_value)) %>%
  ungroup() %>%
  summarize(proportion_over_10 = mean(basket_value > 10))

################
###Question 5###
################
#Which store with over $10K in total sales_value discounts its products the most for loyal customers?

transaction_data %>%
  filter(
    is.finite(regular_price), 
    is.finite(loyalty_price), 
    regular_price > 0
  ) %>%
  mutate(
    pct_loyalty_disc     = 1 - (loyalty_price / regular_price)
  ) %>%
  group_by(store_id) %>%
  summarize(
    total_sales_value    = sum(sales_value), 
    avg_pct_loyalty_disc = mean(pct_loyalty_disc)
  ) %>%
  filter(total_sales_value > 10000) %>%
  arrange(desc(avg_pct_loyalty_disc))

###
###End of Section B###
###