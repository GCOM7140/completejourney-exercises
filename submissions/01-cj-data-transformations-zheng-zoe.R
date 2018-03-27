library(completejourney)
library(ggplot2)
library(ggfortify)
hh_demographic
ggplot(hh_demographic,aes(age_desc))+geom_bar()+coord_flip()

ggplot(hh_demographic,aes(income_desc))+geom_bar(aes(fill= marital_status_code),position = "fill")+coord_flip()





ggplot(product,aes(department,brand))+geom_point()
