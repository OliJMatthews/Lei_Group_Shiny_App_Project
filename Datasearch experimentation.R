Country <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Croatia","Czechia","Denmark","Estonia",
             "Finland","France","Germany","Greece","Hong Kong","Hungary","Iceland","Ireland","Israel","Italy","Japan","Latvia",
             "Lithuania","Luxembourg","Netherlands","New Zealand","Norway","Poland","Portugal","Republic of Korea","Russia",
             "Slovakia","Slovenia","Spain","Sweden","Switzerland","Taiwan","U.K.","U.S.A.","Ukraine")
Codes <- c("AUS","AUT","BLR","BEL","BGR","CAN","CHL","HRV","CZE", "DNK", "EST", "FIN","FRATNP", "DEUTNP", "GRC", "HKG", "HUN",
           "ISL", "IRL", "ISR", "ITA","JPN","LVA","LTU","LUX","NLD","NZL_NP","NOR","POL","PRT","KOR","RUS","SVK", "SLV","ESP",
           "SWE","CHE","TWN","GBR_NP","USA","UKR")
CountryData <- data.frame(Country,Codes)

CountryA <- which(CountryData$Country=="Sweden")
CountryB <- which(CountryData$Country=="Australia")
CodeA <- CountryData$Codes[CountryA]
CodeB <- CountryData$Codes[CountryB]

BirthsA <- readHMDweb(CodeA,"Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
DeathsA <- readHMDweb(CodeA,"Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!")
BirthsB <- readHMDweb(CodeB,"Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
DeathsB <- readHMDweb(CodeB,"Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!")

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

Births <- bind_rows(BirthsA,BirthsB)
Deaths <- bind_rows(DeathsA,DeathsB)
Deaths <- Deaths %>% group_by(Year,Country,Sex) %>% 
  mutate(value=sum(value)) %>% 
  distinct()
