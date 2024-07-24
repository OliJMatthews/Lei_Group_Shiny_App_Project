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
             "ISL", "IRL","ITA","JPN","NLD","NZL_NP","NOR","PRT","SVK","ESP",
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
      Death_Rate = Death_Count / Pop_Count * 1000,
    ) %>%
    ungroup() %>%
    group_by(Year,Country) %>%
    mutate(Birth_Rate = Birth_Count / sum(Pop_Count) * 1000 * 2)
  return(combinedDF)
}
Countries <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Czechia","Denmark",
               "Finland","France","Hungary","Iceland","Ireland","Italy","Japan",
               "Netherlands","New Zealand","Norway","Portugal",
               "Slovakia","Spain","Sweden","Switzerland","U.K.","U.S.A.")

Rates <- reduce(lapply(Countries,calculateRates),rbind)
Rates <- data.frame(Rates) %>% relocate(Country)
write.csv(Rates,"~/Lei_Group_Shiny_App_Project/Rates.csv")

standardisedDR <- function(CountryName){
  CountryCode <- getCountryCode(CountryName)
  deathsDF <- readHMDweb(CountryCode,"Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    mutate(Age_Group = cut(Age,c(seq(0,100,by=5),Inf),inlude.lowest = TRUE, right = FALSE))
  
  age_dist <- data.frame(
    Age_Group = levels(deathsDF$Age_Group),
    Perc = c(8.860,8.690,8.600,8.470,8.220,7.930,7.610,7.150,6.590,6.040,5.370,
             4.550,3.720,2.960,2.210,1.520,0.910,0.440,0.150,0.040,0.005) / 100
  )
  
  deathsDF <- deathsDF %>% 
    left_join(age_dist, by = "Age_Group") %>%
    group_by(Year,Age_Group) %>%
    reframe(
      Year,
      Age_Group,
      Perc,
      Deaths = sum(Total)
    ) 
  
  popDF <- readHMDweb(CountryCode,"Population","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    mutate(Age_Group = cut(Age,c(seq(0,100,by=5),Inf),inlude.lowest = TRUE, right = FALSE)) %>%
    select(Year,Age_Group,Total2) %>%
    group_by(Year,Age_Group) %>%
    reframe(
      Year,
      Age_Group,
      Pop_Total = sum(Total2)
    ) %>%
    unique()
  
  deathsDF <- deathsDF %>%
    inner_join(popDF, by = c("Age_Group","Year")) %>%
    unique() %>%
    group_by(Year) %>%
    reframe(
      Year,
      Age_Group,
      Deaths,
      Death_Rate = Deaths / Pop_Total*1000,
      Perc
    ) %>%
    group_by(Year) %>%
    reframe(
      Year,
      Std_Death_Rate = sum(Death_Rate * Perc)
    ) %>%
    unique() %>%
    mutate( Country = CountryName) %>% 
    relocate(Country)

return(deathsDF)
}

Rates <- reduce(lapply(Countries,standardisedDR),rbind)
write.csv(Rates,"~/Lei_Group_Shiny_App_Project/Std_Death_Rate.csv")