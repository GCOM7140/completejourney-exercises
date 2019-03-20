library(tidyverse)
library(completejourney)

transactions <- transactions %>% 
  mutate(
    retail_disc = abs(retail_disc), 
    coupon_disc = abs(coupon_disc), 
    coupon_match_disc = abs(coupon_match_disc)
)

