library(HMDHFDplus)
library(tidyverse)
# Takes the names of two countries and then returns the birth and death data in each.
country_search <- function(CountryNameA,CountryNameB){
  Country <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Croatia","Czechia","Denmark","Estonia",
             "Finland","France","Germany","Greece","Hong Kong","Hungary","Iceland","Ireland","Israel","Italy","Japan","Latvia",
             "Lithuania","Luxembourg","Netherlands","New Zealand","Norway","Poland","Portugal","Republic of Korea","Russia",
             "Slovakia","Slovenia","Spain","Sweden","Switzerland","Taiwan","U.K.","U.S.A.","Ukraine")
Codes <- c("AUS","AUT","BLR","BEL","BGR","CAN","CHL","HRV","CZE", "DNK", "EST", "FIN","FRATNP", "DEUTNP", "GRC", "HKG", "HUN",
           "ISL", "IRL", "ISR", "ITA","JPN","LVA","LTU","LUX","NLD","NZL_NP","NOR","POL","PRT","KOR","RUS","SVK", "SLV","ESP",
           "SWE","CHE","TWN","GBR_NP","USA","UKR")

CountryData <- data.frame(Country,Codes)
  #Extract country code
  CountryA <- which(CountryData$Country==CountryNameA)
  CountryB <- which(CountryData$Country==CountryNameA)
  CodeA <- CountryData$Codes[CountryA]
  CodeB <- CountryData$Codes[CountryB]
  
  #Fetch data based on country codes
  BirthsA <- readHMDweb(CodeA,"Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
  DeathsA <- readHMDweb(CodeA,"Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!")
  BirthsB <- readHMDweb(CodeB,"Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
  DeathsB <- readHMDweb(CodeB,"Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!")
  DeathRateA <- readHMDweb(CodeA,"Mx_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!")
  DeathRateB <- readHMDweb(CodeB,"Mx_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!")
  
  BirthsA <- BirthsA %>% 
    mutate(Country=Country[CountryA]) %>% 
    pivot_longer(c("Male","Female"),names_to="Sex") %>% 
    mutate(Total=NULL)
  DeathsA <- DeathsA %>% 
    mutate(Country=Country[CountryA]) %>% 
    pivot_longer(c("Male","Female"),names_to="Sex") %>% 
    mutate(Total=NULL,Age=NULL,OpenInterval=NULL)
  BirthsB <- BirthsB %>% 
    mutate(Country=Country[CountryB]) %>% 
    pivot_longer(c("Male","Female"),names_to="Sex") %>% 
    mutate(Total=NULL)
  
  DeathsB <- DeathsB %>% 
    mutate(Country=Country[CountryB]) %>% 
    pivot_longer(c("Male","Female"),names_to="Sex") %>% 
    mutate(Total=NULL,Age=NULL,OpenInterval=NULL)
  
  DeathRateA <- DeathRateA %>% 
    mutate(Country=Country[CountryA]) %>% 
    pivot_longer(c("Male","Female"),names_to="Sex") %>% 
    mutate(Total=NULL,Age=NULL,OpenInterval=NULL)

  DeathRateB <- DeathRateB %>% 
    mutate(Country=Country[CountryA]) %>% 
    pivot_longer(c("Male","Female"),names_to="Sex") %>% 
    mutate(Total=NULL,Age=NULL,OpenInterval=NULL)
  
  Births <- bind_rows(BirthsA,BirthsB)
  Deaths <- bind_rows(DeathsA,DeathsB)
  DeathRate <- bind_rows(DeathRateA,DeathRateB)
  
  Deaths <- Deaths %>% group_by(Year,Country,Sex) %>% 
    mutate(value=sum(value)) %>% 
    distinct()
  
  DeathRate <- DeathRate %>% group_by(Year,Country,Sex) %>% 
    mutate(value=sum(value)) %>% 
    distinct()
  
  return(list("Births" = Births,
              "Deaths" = Deaths,
              "DeathRate" = DeathRate))
}