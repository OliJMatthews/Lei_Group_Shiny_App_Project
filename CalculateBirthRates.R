library(HMDHFDplus)
library(tidyverse)
library(dplyr)
library(ggplot2)
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
  birthsDF <- readHMDweb(CountryCode,"Births","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    group_by(Year) %>%
    summarise(
      Male = sum(Male),
      Female = sum(Female),
      Total= sum(Total)
    ) %>%
    mutate(Country = CountryName) %>%
    pivot_longer(cols = - c(Year,Country),names_to = "Type",values_to = "Birth_Count")

  deathsDF <- readHMDweb(CountryCode,"Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    group_by(Year) %>%
    summarise(
      Male = sum(Male),
      Female = sum(Female),
      Total= sum(Total)
    ) %>%
    mutate(Country = CountryName) %>%
    pivot_longer(cols = - c(Year,Country),names_to = "Type",values_to = "Death_Count")

  popDF <- readHMDweb(CountryCode,"Population","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    group_by(Year) %>%
    select(Year,Female2,Male2,Total2) %>%
    group_by(Year) %>%
    summarise( Male = sum(Male2),
               Female = sum(Female2),
               Total = sum(Total2)) %>%
    mutate(Country = CountryName) %>%
    pivot_longer(cols = - c(Year,Country),names_to = "Type",values_to = "Pop_Count")

  combinedDF <- inner_join(birthsDF,deathsDF,by=c("Year","Country","Type")) %>%
    inner_join(popDF,by=c("Year","Country","Type")) %>%
    group_by(Year,Country,Type) %>%
    summarise(
      Type,
      Birth_Count,
      Death_Count,
      Pop_Count,
      Birth_Rate = Birth_Count / Pop_Count * 1000 , 
      Death_Rate = Death_Count / Pop_Count * 1000 
    )
  return(combinedDF)
}
Countries <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Czechia","Denmark",
               "Finland","France","Hungary","Iceland","Ireland","Italy","Japan",
               "Netherlands","New Zealand","Norway","Portugal",
               "Slovakia","Spain","Sweden","Switzerland","U.K.","U.S.A.")

Rates <- reduce(lapply(Countries,calculateRates),rbind)
Rates <- data.frame(Rates) %>% relocate(Country)
write.csv(Rates,"Rates.csv")
