#### 01 Data Transformation Excersize
install.packages("nycflights13")
library(tidyverse)
library(nycflights13)
?flights
flights <-flights
nrow(flights)
##### Question 1

#How many flights flew into LAX?
nrow(filter( flights, dest == "LAX"))
### Answer = 16174

#How many flights flew out of LAX?
nrow(filter( flights, origin == "LAX"))
## Answer = 0

#  How many flights are greater than or equal to 2000 miles?
nrow(filter( flights, distance >= 2000))

# Answer = 51695

#  How many flights were destined for airports in the Los Angeles area (LAX, ONT, SNA, PSP, SBD, BUR, or LGB), but did not originate out of JFK?

nrow(filter( flights, dest %in% c("LAX", "ONT", "SNA", "PSP", "SBD", "BUR", "LGB"), origin != "JFK"))

### Answer = 5737

#### Question 2: How many flights were "ghost flights"? A "ghost flight" is defined as a flight that departed, but never arrived (i.e., has a missing arr_time).
nrow(filter( flights, is.na(arr_time) ))
?is.na

### Answer = 8713

#### Question 3: How does arrange() treat missing values? How could you sort all rows with a missing arr_time to the top of the dataset?
arrange(flights, arr_time)
arrange(flights, desc(arr_time))
flights1
# arrange() ignores missing values and sticks them at the bottom even if you use desc()
arrange(flights, !is.na(desc(arr_time)))

#### Question 4: What do you observe after running the code below? How does this behavior reflect how select() helpers deal with uppercase and lowercase matching by default? How can you change that default?

select(flights, contains("TIME"))

#Error in select(flights, contains("TIME")) : 
# unused argument (contains("TIME"))

#### I'm not sure what to do here?
flights %^%
select(contains("time"))
?contains
select()
?select

####   Question 5: For each destination greater than or equal to 2000 miles away, compute total minutes of departure delay. Then determine what proportion of total-departure-delay minutes each destination represents. What three destinations top the list?

q5 <- filter(flights, distance >= 2000, dep_delay >0)
sum(q5$dep_delay)

# Answer = 667,329 minutes

q5 <- group_by(q5, dest)
q5sum <- q5 %>%
summarize(sum = sum(dep_delay))
q5sum
?mutate
q5sum <- mutate(q5sum, Proportion = sum/667329)
q5sum <- arrange(q5sum, desc(Proportion))

# SFO, LAX, and LAS are the most delayed


###############################################
##### Data Transformation Exercises

library(tidyverse)
library(completejourney)


?select
transaction_data <-
  transaction_data %>% 
  arrange(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time
  )


#### Question 1: Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive.
transaction_data <- mutate(transaction_data, retail_disc = abs(retail_disc), coupon_disc = abs(coupon_disc), coupon_match_disc = abs(coupon_match_disc))
transaction_data


##### Question 2: Create three new variables named regular_price, loyalty_price, and coupon_price according to the following logic:
transaction_data <- mutate(transaction_data, regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
       loyalty_price = (sales_value + coupon_match_disc) / quantity,
       coupon_price  = (sales_value - coupon_disc) / quantity)

####Question 3: transaction_data includes 92,339 unique product IDs. How many of these products (not transactions!) had a regular price of one dollar or less? What does this count equal for loyalty and coupon prices?

reg_price_1_or_less <- filter(transaction_data, regular_price <= 1)

n_distinct(reg_price_1_or_less$product_id)
n_distinct(reg_price_1_or_less$loyalty_price)
n_distinct(reg_price_1_or_less$coupon_price)

# 12442 products, 17338 loyalty prices, 17531 coupon prices

#### Question 4: What proportion of baskets are over $10 in sales value?

baskets_over_10 <- group_by(transaction_data, basket_id)
baskets_over_10 <- summarize(baskets_over_10, basket_value = sum(sales_value))
baskets_over_10 <- filter(baskets_over_10, basket_value > 10)
n_distinct(baskets_over_10$basket_id) / n_distinct(transaction_data$basket_id)
# 65.3% of baskets are over $10

#### Question 5: Which store with over $10K in total sales_value discounts its products the most for loyal customers?
sales_value_over_10k <- group_by(transaction_data, store_id)
sales_value_over_10k <- summarise(sales_value_over_10k, sales_value_sum = sum(sales_value), pct_loyalty_disc = 1 - (sum(loyalty_price) / sum(regular_price)))
sales_value_over_10k <-  filter(sales_value_over_10k, sales_value_sum > 10000)
sales_value_over_10k <- arrange(sales_value_over_10k, desc(pct_loyalty_disc))
sales_value_over_10k



