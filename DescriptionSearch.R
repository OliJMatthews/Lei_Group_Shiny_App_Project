library(tidyverse)
# test_df <- data.frame(
#   Year = c(1989),
#   Country = c("China"),
#   Description = c("Tianamen Square")
# )

descriptionSearch <- function(data, Year, Country){
  description <- data %>% 
  filter(Year == Year && Country == Country) %>% 
  select(Description)
  return(description$Description)
}

descriptionSearch(test_df,1989,China)
