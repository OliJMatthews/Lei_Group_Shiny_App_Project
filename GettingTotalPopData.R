
getCountryCode <- function(countryName){
  Countries <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Croatia","Czechia","Denmark","Estonia",
                 "Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Latvia",
                 "Lithuania","Luxembourg","Netherlands","New Zealand","Norway","Poland","Portugal","Republic of Korea","Russia",
                 "Slovakia","Slovenia","Spain","Sweden","Switzerland","Taiwan","U.K.","U.S.A.","Ukraine")
  Codes <- c("AUS","AUT","BLR","BEL","BGR","CAN","CHL","HRV","CZE", "DNK", "EST", "FIN","FRATNP", "DEUTNP", "GRC", "HUN",
             "ISL", "IRL", "ISR", "ITA","JPN","LVA","LTU","LUX","NLD","NZL_NP","NOR","POL","PRT","KOR","RUS","SVK", "SLV","ESP",
             "SWE","CHE","TWN","GBR_NP","USA","UKR")
  
  countryIndex <- which(Countries == countryName)
  if (length(countryIndex) == 0) {
    warning("Country name is invalid")
    return(NULL)
  }else{
    return(Codes[countryIndex])
  }
}


gettotalpopdata <-function(country){
  readHMDweb(getCountryCode(country),"Population","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    select(-Female1,-Male1,-Total1) %>%
    mutate(Male = Male2,Female = Female2) %>%
    select(-Female2,-Male2) %>%
    pivot_longer(cols = c(Male,Female),
                 names_to = "Sex",
                 values_to = "Population") %>%
    group_by(Year) %>% 
    mutate(Total=sum(Population)) %>% 
    select(Year,Total) %>% 
    distinct() %>% 
    mutate(Country=country) %>% 
    select(Country,Year,Total)}
totalpopdata <- bind_rows(gettotalpopdata("U.S.A."),
                          gettotalpopdata("U.K."),
                          gettotalpopdata("Switzerland"),
                          gettotalpopdata("Sweden"),
                          gettotalpopdata("Spain"),
                          gettotalpopdata("Slovakia"),
                          gettotalpopdata("Portugal"),
                          gettotalpopdata("Norway"),
                          gettotalpopdata("New Zealand"),
                          gettotalpopdata("Netherlands"),
                          gettotalpopdata("Japan"),
                          gettotalpopdata("Italy"),
                          gettotalpopdata("Ireland"),
                          gettotalpopdata("Iceland"),
                          gettotalpopdata("Hungary"),
                          gettotalpopdata("France"),
                          gettotalpopdata("Finland"),
                          gettotalpopdata("Denmark"),
                          gettotalpopdata("Czechia"),
                          gettotalpopdata("Chile"),
                          gettotalpopdata("Canada"),
                          gettotalpopdata("Bulgaria"),
                          gettotalpopdata("Belgium"),
                          gettotalpopdata("Belarus"),
                          gettotalpopdata("Austria"),
                          gettotalpopdata("Australia"))

write.csv(totalpopdata,"TotalPopulationData")
#"U.S.A.","U.K.","Ukraine","Taiwan","Switzerland","Sweden","Spain","Republic of Korea","Slovakia","Slovenia","Russia",
#"Portugal","Poland","Norway","New Zealand","Netherlands","Luxembourg","Lithuania","Latvia","Japan","Italy","Israel",
#"Ireland","Iceland","Hungary","Greece","Germany","France","Finland","Estonia","Denmark",
#"Czechia","Croatia","Chile","Canada","Bulgaria","Belgium","Belarus","Austria","Australia"
