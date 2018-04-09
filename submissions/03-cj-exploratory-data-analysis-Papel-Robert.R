#' ---
#' title: "Answers to the completejourney 03 exploratory analysis Exercise"
#' author: Robert Papel
#' date: April 9, 2018
#' output: github_document
#' ---

install.packages("devtools")
devtools::install_github('GCOM7140/completejourney', 
                         auth_token = '07886c312886ca2475fd4e55836e985a20286193')
library(tidyverse)
library(completejourney)

#' **Exercise 1**: How many unique households exist in my_transaction_data, and
#' how many of these households in my_transaction_data have demographic data in
#' hh_demographic?

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

my_transaction_data %>% 
  distinct(household_key) %>% 
  nrow()

#' 2500, with the code copied over from github

inner_join(my_transaction_data, hh_demographic) %>% 
  distinct(household_key) %>% 
  nrow()

#' 801, with the code copied over from github

#' **Exercise 2**: Determine median weekly spend per individual using the
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

exercise_2 %>% 
  summarize(
    med_wkly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)
  )
#' The median weekly spending per individual is $19.4, but I am confused because
#' you put the answers as the hints for the exercises???

#' **Exercise 3**:Building on Exercise 2, plot median spend per individual for
#' the five household sizes in my_transaction_data.

exercise_2 %>% 
  group_by(hh_size) %>% 
  summarize(
    med_wkly_spend_per_ind = median(wkly_spend_per_ind, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = hh_size, y = med_wkly_spend_per_ind)) +
  geom_col()
#' Spending goes down as the househould size gets bigger, as seen in the bar
#' plot created with the code above. If the household has one person, they spend
#' more per week on themselves.

#' **Exercise 4**:Are baskets with diapers in them more likely than average to
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
  )

inner_join(my_transaction_data, product) %>% 
  mutate(
    diapers = sub_commodity_desc == 'BABY DIAPERS', 
    beer    = sub_commodity_desc == 'BEERALEMALT LIQUORS'
  ) %>%
  group_by(basket_id) %>%
  summarize(
    basket_has_diapers = max(diapers), 
    basket_has_beer    = max(beer)
  ) %>% 
  summarize(
    prop_both   = sum(basket_has_diapers * basket_has_beer == 1) / 
      sum(basket_has_diapers == 1),
    prob_beer   = mean(basket_has_beer),
    diaper_lift = prop_both / prob_beer
  )
#' I would say that the probability of having both beer and diapers is
#' practically significant, as 1) some dad is probably shopping on his way home
#' from work and is stressed out, and 2) the diaper_lift number is 1.06, which I
#' think is statistically significant


#' **Exercise 5**:Using a stacked bar chart that's partitioned by income level
#' (i.e., income_desc), visualize the total amount of money customers spent on
#' national-brand products versus private-label products.

inner_join(my_transaction_data, hh_demographic) %>% 
  mutate(
    income_desc = factor(income_desc, 
                         levels = c('Under 15K',   '15-24K',   '25-34K', 
                                    '35-49K',   '50-74K',   '75-99K', 
                                    '100-124K', '125-149K', '150-174K', 
                                    '175-199K', '200-249K',    '250K+'),
                         ordered = TRUE)
  ) %>%
  group_by(income_desc, brand) %>%
  summarize(total_spend = sum(purchase_price)) %>% 
  ggplot() +
  geom_col(aes(x = income_desc, y = total_spend, fill = brand), 
           position = 'fill') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#' The more people make per household, the less they spend on private brands,
#' which is interesting to me, as arent those private brands more expensive than
#' national brands? I'm not sure