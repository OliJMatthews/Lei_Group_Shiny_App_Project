library(tidyverse)
test_df <- data.frame(
  Year = c(1989,2000),
  Country = c("China","DPRK"),
  Description = c("Tianamen Square","KIM JONG UM")
)

descriptionSearch <- function(data, year, country){
  description <- data %>% 
    filter(Year == year) %>% 
    filter(Country==country)
  result <- description$Description
return(result)
}

descriptionSearch(test_df,2000,"DPRK")
