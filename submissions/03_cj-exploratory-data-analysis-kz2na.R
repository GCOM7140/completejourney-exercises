library(tidyverse)
library(completejourney)
# Begining with creating transactions_prices:

transactions %>% 
  filter(quantity != 0) %>%
  mutate(
    price_regular  = (sales_value + retail_disc + coupon_match_disc) /
      quantity,
    price_loyalty  = (sales_value + coupon_match_disc) / 
      quantity,
    price_coupon   = (sales_value - coupon_disc) / 
      quantity,
    price_purchase = case_when(
      coupon_disc > 0 ~ price_coupon, 
      retail_disc > 0 ~ price_loyalty,
      TRUE            ~ price_regular
    )
  ) -> 
  transactions_prices

# Question 1: Determine median weekly spend per individual (not household) using price_purchase intransactions_prices and household_size in demographics.

transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
      as.integer()
  )%>%
  group_by(household_id,week)%>%
  mutate(house_week=sum(price_purchase)/household_size)%>%
  ungroup()%>%
  summarize(median(house_week)) 
# 44.2

# Question 2: Building on Question 2, plot median spend per individual by household size.
transactions_prices %>%
  inner_join(demographics, by = "household_id") %>% 
  mutate(
    household_size = str_replace(household_size, "5\\+", "5") %>% 
      as.integer()
  )%>%
  group_by(household_id,week)%>%
  mutate(house_week=sum(price_purchase)/household_size)%>%
  ungroup()%>%
  group_by(household_size)%>%
  summarize(med=median(house_week))%>%
  ggplot(aes(household_size,med))+
  geom_bar(stat = "identity")+
  geom_text(aes(label= med), stat = "identity")



# Question 3: Are baskets with diapers in them more likely than average to have beer in them, too? Legend has it that placing these two product categories closer together can increase beer sales (Powers 2002). Using the following starter code, calculate lift for the “association rule” that diapers in a basket (i.e., product_type == "BABY DIAPERS") imply that beer is in the basket (i.e., product_type == "BEERALEMALT LIQUORS"). Does the association between these products offer support for the legend?

transactions_prices %>% 
  inner_join(products, by = "product_id") %>% 
  mutate(diapers = product_type == "BABY DIAPERS", 
         beer    = product_type == "BEERALEMALT LIQUORS")%>%
  group_by(basket_id) %>%
  summarize(basket_has_diapers = max(diapers),
            basket_has_beer    = max(beer)) %>%
  ungroup()%>%
  mutate(have_both = ifelse(basket_has_diapers==T & basket_has_beer ==T,1,0))%>%
  summarize(sum(have_both,na.rm=T)/sum(basket_has_diapers,na.rm=T),mean(basket_has_beer,na.rm=T))

#Among baskets with diapers, 5.52% have beers. It is the similar ratio observed in the entire population. 

# Question 4: Using a stacked bar chart that is partitioned by income level (i.e., income), visualize the total amount of money that households in the Complete Journey Study spent on national-brand products versus private-label products (i.e., brand).

transactions_prices %>% 
  left_join(demographics, by = "household_id") %>% 
  left_join(products, by = "product_id")%>%
  # filter(!is.na(income))%>%
  ggplot(aes(income,sum(price_purchase),stat= "identity"))+
  geom_col(aes(fill=brand),position="fill")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




