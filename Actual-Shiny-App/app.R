library(shiny)
library(tidyverse)
library(bslib)
library(HMDHFDplus)
library(leaflet)
library(sf)
library(rnaturalearth)

Country <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Croatia","Czechia","Denmark","Estonia",
             "Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Latvia",
             "Lithuania","Luxembourg","Netherlands","New Zealand","Norway","Poland","Portugal","Republic of Korea","Russia",
             "Slovakia","Slovenia","Spain","Sweden","Switzerland","Taiwan","U.K.","U.S.A.","Ukraine")
world <- ne_countries(scale = "medium", returnclass = "sf",country=append(Country,c("United Kingdom","United States of America","South Korea")))
# This function takes two countries and returns three date frames: Births, Deaths, Deathrates (Needs things, lots of NAs)
country_search <- function(CountryNameA,CountryNameB){
  Country <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Croatia","Czechia","Denmark","Estonia",
               "Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Latvia",
               "Lithuania","Luxembourg","Netherlands","New Zealand","Norway","Poland","Portugal","Republic of Korea","Russia",
               "Slovakia","Slovenia","Spain","Sweden","Switzerland","Taiwan","U.K.","U.S.A.","Ukraine")
  Codes <- c("AUS","AUT","BLR","BEL","BGR","CAN","CHL","HRV","CZE", "DNK", "EST", "FIN","FRATNP", "DEUTNP", "GRC", "HUN",
             "ISL", "IRL", "ISR", "ITA","JPN","LVA","LTU","LUX","NLD","NZL_NP","NOR","POL","PRT","KOR","RUS","SVK", "SLV","ESP",
             "SWE","CHE","TWN","GBR_NP","USA","UKR")
  CountryData <- data.frame(Country,Codes)
  #Extract country code
  CountryA <- which(CountryData$Country==CountryNameA)
  CountryB <- which(CountryData$Country==CountryNameB)
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
    mutate(Country=Country[CountryB]) %>% 
    pivot_longer(c("Male","Female"),names_to="Sex") %>% 
    mutate(Total=NULL,Age=NULL,OpenInterval=NULL)
  
  Births <- bind_rows(BirthsA,BirthsB)
  Deaths <- bind_rows(DeathsA,DeathsB)
  DeathRate <- bind_rows(DeathRateA,DeathRateB)
  
  Deaths <- Deaths %>% group_by(Year,Country,Sex) %>% 
    mutate(value=sum(value)) %>% 
    distinct()
  
  DeathRate <- DeathRate %>% group_by(Year,Country,Sex) %>% 
    mutate(value=mean(value)) %>% 
    distinct()
  
  return(list("Births" = Births,
              "Deaths" = Deaths,
              "DeathRate" = DeathRate))
}
# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "Human Mortality Database",
  bg = "#0062cc",
  underline = TRUE,
  nav_panel(title = "Welcome", p("First tab content.")),
  nav_panel(title = "Interactive Map",
            p(leafletOutput("map"),
              sliderInput("year","Year:",value=2000,min=1900,max=2024,step=1,sep="",width="100%"),
              textOutput("country_name"))),
  nav_panel(title = "Simulation", 
            p(sidebarLayout(
              sidebarPanel(
                checkboxGroupInput("sex","Sex:",c("Male","Female")),
                selectInput("country","Country (select 2):",Country,multiple=TRUE),
                radioButtons("type","Data:",c("Births","Deaths","Death Rates"))
              ),
              mainPanel(
                tableOutput("table")
              )
            )))
)

server <- function(input, output) {
  data.sets <- reactive({
    input_valueA <- input$country[1]
    input_valueB <- input$country[2]
    result <- country_search(input_valueA,input_valueB)
    return(result)
  })
  
#  chosen.data.set <- reactive({
 #   if(input$type=="Births"){chosen.data.set <- data.sets()$Births}  THIS DOSENT WORK YET
 #   if(input$type=="Deaths"){chosen.data.set <- data.sets()$Deaths}
#    if(input$type=="Death Rates"){chosen.data.set <- data.sets()$DeathRates}
 # })
  
  output$table <-renderTable(data.sets()$Births)
  
  output$map <- renderLeaflet({
    leaflet(world) %>%
      addTiles() %>%
      addPolygons(data=world,color = c("green"), weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  layerId = c("U.S.A.","U.K.","Ukraine","Taiwan","Switzerland","Sweden","Spain","Republic of Korea","Slovakia","Slovenia","Russia",
                              "Portugal","Poland","Norway","New Zealand","Netherlands","Luxembourg","Lithuania","Latvia","Japan","Italy","Israel",
                              "Ireland","Iceland","Hungary","Greece","Germany","France","Finland","Estonia","Denmark",
                              "Czechia","Croatia","Chile","Canada","Bulgaria","Belgium,","Belarus","Austria","Australia"))
  })
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    country_name <- click$id  # Extract the clicked country name
    output$country_name <- renderText({
      paste("You clicked on:", country_name)})})
}

# Run the application 
shinyApp(ui = ui, server = server)