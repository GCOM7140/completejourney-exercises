# install.packages('devtools')
library(devtools)
library(tidyverse)
# devtools::install_github('GCOM7140/completejourney', 
#                auth_token = '')
library(completejourney)

# use select() to organize the columns systematically / help you understand them
# better

(transaction <- transaction_data %>% 
  select(
    quantity,
    sales_value, 
    retail_disc, coupon_disc, coupon_match_disc,
    household_key, store_id, basket_id, product_id, 
    week_no, day, trans_time
  ) %>% 
  mutate(
   retail_disc       = abs(retail_disc),
   coupon_disc       = abs(coupon_disc),
   coupon_match_disc = abs(coupon_match_disc)
  )
)

# filter the rows down to those that were discounted due to a manufacturer's 
# coupon and then do a count of rows.

transaction %>% 
  filter(coupon_disc > 0) %>% 
  nrow()

# how many of the transactions that were discounted due to a manufacturer's
# coupon DID NOT RECEIVE ANY FORM OF A DISCOUNT MATCH from the retailer?

transaction %>% 
  filter(coupon_disc > 0, coupon_match_disc == 0) %>% 
  nrow()

# how many of the transactions that were discounted due to a manufacturer's
# coupon DID RECEIVE SOME FORM OF A DISCOUNT MATCH from the retailer?

transaction %>% 
  filter(coupon_disc > 0, coupon_match_disc > 0) %>% 
  nrow()

# how many of the transactions that were discounted due to a manufacturer's 
# coupon RECEIVED AN EXACT DISCOUNT MATCH from the retailer?

transaction %>% 
  filter(coupon_disc > 0, coupon_disc < coupon_match_disc) %>% 
  nrow()

# join the transaction data with product metadata (see chapter 13 of r4ds)

(transaction <- left_join(transaction, product, by = "product_id"))

# use filter() and arrange() to recreate the example table on page 4 of the 
# complete journey user guide.

(example_table <- transaction %>% 
  filter(
    product_id == 819063,
    basket_id %in% c(35730137393, 41756231898, 36027750817)
  ) %>% 
  arrange(desc(household_key)) %>% 
  select(commodity_desc, everything())
)

# according to the logic on page 4 of the complete journey user guide, use
# mutate() to make new variables in the table for regular_price and
# loyalty_price. use select() to move these variables to the left-hand side of 
# the table.

example_table %>% 
  mutate(
    regular_price     = (sales_value + retail_disc + coupon_match_disc) /
                        quantity,
    loyalty_price     = (sales_value + coupon_match_disc) /
                        quantity,
    coupon_price      = (sales_value - coupon_disc) /
                        quantity
  ) %>% 
  select(regular_price, loyalty_price, coupon_price, quantity, sales_value, 
         retail_disc, coupon_disc, coupon_match_disc, everything( ))

# kroger basket exercise

kroger_basket <- tribble(
  ~quantity, ~sales_value, ~retail_disc, ~coupon_disc, ~coupon_match_disc, 
  ~household_key, ~store_id, ~basket_id, ~product, ~week_no, ~day, ~trans_time,
 #-|-----|-----|-----|-----|-----|----|------------|-----------|---|---|-------|
  2, 5.48, 0.00, 0.50, 0.50, 2501, 563, 99999999999, "cereal"  , 11, 11, "2101",
  3, 3.00, 0.75, 0.00, 0.00, 2501, 563, 99999999999, "sprt drk", 11, 11, "2101",
  1, 3.79, 0.00, 0.00, 0.00, 2501, 563, 99999999999, "red bull", 11, 11, "2101",
  2, 4.00, 0.98, 0.00, 0.00, 2501, 563, 99999999999, "chips"   , 11, 11, "2101",
  1, 3.29, 0.70, 0.00, 0.00, 2501, 563, 99999999999, "hummus"  , 11, 11, "2101"
)

kroger_basket %>% 
  mutate(
    regular_price     = (sales_value + retail_disc + coupon_match_disc) /
                        quantity,
    loyalty_price     = (sales_value + coupon_match_disc) / 
                        quantity,
    coupon_price      = (sales_value - coupon_disc) / 
                        quantity,
    purchase_price    = ifelse(coupon_disc > 0, coupon_price, 
                               ifelse(retail_disc > 0, loyalty_price,
                                      regular_price)),
    purchase_disc     = regular_price - purchase_price,
    sales_value_check = purchase_price*quantity + coupon_disc
  ) %>% 
  select(
    purchase_price, purchase_disc, regular_price, loyalty_price, coupon_price,
    everything()
  )

transaction <- transaction %>% 
  # filtering out transactions where quantity is equal to zero or more than 50
  filter(
    quantity != 0, quantity < 50
  ) %>%
  # creating new variables related to per-item prices and discount from 
  mutate(
    regular_price     = (sales_value + retail_disc + coupon_match_disc) /
                        quantity,
    loyalty_price     = (sales_value + coupon_match_disc) / 
                        quantity,
    coupon_price      = (sales_value - coupon_disc) / 
                        quantity,
    purchase_price    = ifelse(coupon_disc > 0, coupon_price, 
                               ifelse(retail_disc > 0, loyalty_price,
                                      regular_price)),
    purchase_disc     = regular_price - purchase_price,
    sales_value_check = purchase_price*quantity + coupon_disc
  ) %>% 
  select(
    purchase_price, purchase_disc, regular_price, loyalty_price, coupon_price,
    everything()
  )


# ensuring that the price variables were defined as intended, then removing sales_value_check from the data (use command, shift, and / to reflow a comment)
near(transaction$sales_value_check, transaction$sales_value) %>% 
  all()

transaction %>% 
  select(-sales_value_check)

# visualizing the complete journey data

transaction %>% 
  filter(department != "GROCERY") %>% 
  ggplot(aes(department)) + 
  geom_bar() + 
  coord_flip()

# Burhan's bar chart of customer ages
ggplot(hh_demographic, aes(age_desc)) +
  geom_bar(alpha = .8, fill = "blue", col = "black") + 
  theme_bw()


