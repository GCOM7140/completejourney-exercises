#' ---
#' title: "Answers to the CJ Exploratory Data Analysis Exercise"
#' author: Noah Rodammer
#' date: April 11, 2018
#' output: github_document
#' ---

library(tidyverse)
library(completejourney)

#' organize columns systematically
left_join(transaction_data, product) %>% 
  left_join(hh_demographic) %>% 
  filter(
    quantity != 0
  ) %>% 
  mutate(
    regular_price  = (sales_value + retail_disc + coupon_match_disc) /
      quantity,
    loyalty_price  = (sales_value + coupon_match_disc) / 
      quantity,
    coupon_price   = (sales_value - coupon_disc) / 
      quantity,
    purchase_price = ifelse(coupon_disc > 0, coupon_price, 
                            ifelse(retail_disc > 0, loyalty_price,
                                   regular_price))
  ) -> my_transaction_data

#' **Question 1**:How many unique households exist in my_transaction_data, and 
#' how many of these households in my_transaction_data have demographic data in 
#' hh_demographic?

#' 1. 'Use distinct() to create a tibble of unique household_key values.
#' 2. Use nrow() to count these households.
#' 3. Use inner_join() to match my_transaction_data with hh_demographic.
#' 4. Use distinct() and nrow() to count the rows that remain.

my_transaction_data %>%
  distinct(household_key) %>%
  nrow() 

#' 2,500 houses

inner_join(my_transaction_data, hh_demographic) %>% 
  distinct(household_key) %>%
  nrow()

#' 801 houses

#' **Question 2**:Determine median weekly spend per individual using the 
#' following tibble (i.e., exercise_2).


inner_join(my_transaction_data, hh_demographic) %>% 
  mutate(
    hh_size          = str_replace(household_size_desc, '5\\+', '5') %>% 
      as.integer()
  ) %>% 
  group_by(household_key, week_no) %>% 
  summarize(
    total_spend      = sum(purchase_price, na.rm = TRUE),
    hh_size          = max(hh_size,        na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  mutate(
    wkly_spend_per_ind = total_spend / hh_size
  ) -> exercise_2

median(exercise_2$wkly_spend_per_ind,na.rm=TRUE)
#$19.43


#' **Question 3**:Building on Exercise 2, plot median spend per individual for 
#' the five household sizes in my_transaction_data.


exercise_2 %>% 
  group_by(hh_size) %>% 
  summarize(
    householdMed      = median(wkly_spend_per_ind, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x=hh_size,y=householdMed)) + 
   geom_col() 




#' **Question 4**: Are baskets with diapers in them more likely than average to 
#' have beer in them too? Legend has it that placing these two product 
#' categories closer together can increase beer sales (Powers 2002). Using the 
#' following starter code, calculate lift for the "association rule" that 
#' diapers in a basket (i.e., sub_commodity_desc == 'BABY DIAPERS') imply beer 
#' is in the basket (i.e., sub_commodity_desc == 'BEERALEMALT LIQUORS'). Is the 
#' association between these products practically significant in 
#' my_transaction_data?

inner_join(my_transaction_data, product) %>% 
  mutate(
    diapers = sub_commodity_desc == 'BABY DIAPERS', 
    beer    = sub_commodity_desc == 'BEERALEMALT LIQUORS'
          ) %>%
  group_by(basket_id) %>%
  summarize(
    hasDiaper = max(diapers),
    hasBeer = max(beer)
  )%>%
summarize(
  propDiaper=mean(hasDiaper),
  propBeer=mean(hasBeer),
  given = sum(hasDiaper*hasBeer)/sum(hasDiaper),
  lift=given/propBeer
)
  
#' The lift for diaper is greater than 1, which implies
#' implies that the relationship between the antecedent 
#' and the consequent is more significant than would
#' be expected if the two sets were independent



#' **Question 5:**: Using a stacked bar chart that's partitioned by income 
#' level (i.e., income_desc), visualize the total amount of money customers 
#' spent on national-brand products versus private-label products.

inner_join(my_transaction_data, hh_demographic) %>% 
  mutate(
    income_desc = factor(income_desc, 
                            levels=
                              c("Under 15K", "15-24K", "25-34K", "35-49K",
                                "50-74K","75-99K","100-124K","125-149K",
                                "150-174K","175-199K","200-249K","250K+"),
                            ordered=TRUE)
  ) %>%
  group_by(brand,income_desc) %>% 
  summarize(
    price      = sum(purchase_price, na.rm = TRUE)
  ) %>% 
ggplot(aes(income_desc, price)) +
  geom_bar(stat="identity",aes(fill=brand),position="fill") 

#'Above is adjusted from 0 - 1

inner_join(my_transaction_data, hh_demographic) %>% 
  mutate(
    income_desc = factor(income_desc, 
                         levels=
                           c("Under 15K", "15-24K", "25-34K", "35-49K",
                             "50-74K","75-99K","100-124K","125-149K",
                             "150-174K","175-199K","200-249K","250K+"),
                         ordered=TRUE)
  ) %>%
  group_by(brand,income_desc) %>% 
  summarize(
    price      = sum(purchase_price, na.rm = TRUE)
  ) %>% 
  ggplot(aes(income_desc, price)) +
  geom_bar(stat="identity",aes(fill=brand)) 

#'Above is unadjusted and shows total amount spent by the income group



