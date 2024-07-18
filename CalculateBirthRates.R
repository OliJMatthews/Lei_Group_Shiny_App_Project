library(HMDHFDplus)
library(tidyverse)
library(dplyr)
getCountryCode <- function(countryName){
  Countries <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Czechia","Denmark",
                 "Finland","France","Hungary","Iceland","Ireland","Italy","Japan",
                 "Netherlands","New Zealand","Norway","Portugal",
                 "Slovakia","Spain","Sweden","Switzerland","U.K.","U.S.A.")
  Codes <- c("AUS","AUT","BLR","BEL","BGR","CAN","CHL","CZE", "DNK",  "FIN","FRATNP",  "HUN",
             "ISL", "IRL",  "ITA","JPN","NLD","NZL_NP","NOR","PRT","SVK","ESP",
             "SWE","CHE","GBR_NP","USA")
  
  countryIndex <- which(Countries == countryName)
  if (length(countryIndex) == 0) {
    warning("Country name is invalid")
    return(NULL)
  }else{
    return(Codes[countryIndex])
  }
}

Countries <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Czechia","Denmark",
               "Finland","France","Hungary","Iceland","Ireland","Italy","Japan",
               "Netherlands","New Zealand","Norway","Portugal",
               "Slovakia","Spain","Sweden","Switzerland","U.K.","U.S.A.")

calculateRates <- function(CountryName){
  CountryCode <- getCountryCode(CountryName)
  Births <- readHMDweb(CountryCode,"Births","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    group_by(Year) %>%
    summarise(
      Male_Births = sum(Male),
      Female_Births = sum(Female),
      Total_Births = sum(Total)
    ) %>%
    mutate(Country = CountryName) 
  
  Deaths <- readHMDweb(CountryCode,"Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    group_by(Year) %>%
    mutate(Country = CountryName) %>%
    summarise(
      Male_Deaths = sum(Male),
      Female_Deaths = sum(Female),
      Total_Deaths = sum(Total)
    )
  
  Pop <- readHMDweb(CountryCode,"Population","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    select(Year,Female2,Male2,Total2) %>%
    group_by(Year) %>%
    summarise( Male_Pop = sum(Male2),
              Female_Pop = sum(Female2),
              Total_Pop = sum(Total2))

  combined <- inner_join(Deaths,Pop, by="Year") %>%
    inner_join(Births, by = "Year") %>%
    mutate(Male_Birth_Rate = Male_Births / Total_Pop * 1000) %>%
    mutate(Female_Birth_Rate = Female_Births / Total_Pop * 1000) %>%
    mutate(Total_Birth_Rate = Total_Births / Total_Pop * 1000) %>%
    mutate(Male_Death_rate = Male_Deaths/ Total_Pop * 1000) %>%
    mutate(Female_Death_rate = Female_Deaths/ Total_Pop * 1000) %>%
    mutate(Total_Death_rate = Total_Deaths / Total_Pop * 1000)

  return(combined)
}

data <- reduce(lapply(Countries,calculateRates),rbind)
save(data, file = "Rates.csv")