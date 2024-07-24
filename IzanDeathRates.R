library("tidyverse")
Std_Death_Rate <- read_csv("Std_Death_Rate.csv", 
                           col_types = cols(...1 = col_skip()))

data <- Std_Death_Rate %>% 
  filter(Country=="U.K."|Country=="Japan") %>% 
  filter(Year>1949 & Year<2019)

ggplot(data)+
  geom_line(aes(x=Year,y=Std_Death_Rate,color=Country))+
  ggtitle("Age-Standardised Death Rate against Time")+
  ylab("Age-Standardised Death Rate")
