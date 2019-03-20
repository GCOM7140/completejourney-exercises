## Oliver Song (os8ua)

library(tidyverse)
library(completejourney)

## Question 1 - Change the discount variables (i.e., retail_disc, coupon_disc, coupon_match_disc) from negative to positive

transactions %>%
  mutate(retail_disc = abs(retail_disc),
         coupon_disc = abs(coupon_disc),
         coupon_match_disc = abs(coupon_match_disc))


