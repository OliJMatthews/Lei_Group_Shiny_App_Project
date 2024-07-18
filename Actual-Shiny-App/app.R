library(shiny)
library(tidyverse)
library(bslib)
library(HMDHFDplus)
library(leaflet)
library(sf)
library(rnaturalearth)
library(RColorBrewer)

test_df <- data.frame(
  Year = c("1989","2000"),
  Country = c("Taiwan","U.K."),
  Description = c("TestA","TestB")
)
Country <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Czechia","Denmark",
             "Finland","France","Hungary","Iceland","Ireland","Italy","Japan",
             "Netherlands","New Zealand","Norway","Portugal",
             "Slovakia","Spain","Sweden","Switzerland","U.K.","U.S.A.")
world <- ne_countries(scale = "medium", returnclass = "sf",country=append(Country,c("United Kingdom","United States of America")))
TotalPopulationData <- read_csv("TotalPopulationData",col_types = cols(...1 = col_skip()))
country_search <- function(CountryNameA,CountryNameB){
  Country <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Czechia","Denmark",
               "Finland","France","Hungary","Iceland","Ireland","Italy","Japan",
               "Netherlands","New Zealand","Norway","Portugal",
               "Slovakia","Spain","Sweden","Switzerland","U.K.","U.S.A.")
  Codes <- c("AUS","AUT","BLR","BEL","BGR","CAN","CHL","CZE", "DNK",  "FIN","FRATNP",  "HUN",
             "ISL", "IRL",  "ITA","JPN","NLD","NZL_NP","NOR","PRT","SVK","ESP",
             "SWE","CHE","GBR_NP","USA")
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
  
  Births <- bind_rows(BirthsA,BirthsB)
  Deaths <- bind_rows(DeathsA,DeathsB)
  
  Deaths <- Deaths %>% group_by(Year,Country,Sex) %>% 
    mutate(value=sum(value)) %>% 
    distinct()
  
  
  return(list("Births" = Births,
              "Deaths" = Deaths))
}
description <- function(df,year,country){
  a <- filter(df,Year==year & Country==country)
  return(a$Description)
}
get_age_pyramid_data <- function(countryCode){
  data <-readHMDweb(countryCode,"Population","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    select(-Female1,-Male1,-Total1) %>%
    mutate(Male = Male2,Female = Female2) %>%
    select(-Female2,-Male2) %>%
    pivot_longer(cols = c(Male,Female),
                 names_to = "Sex",
                 values_to = "Population") %>%
    select(Year,Age,Sex,Population)
  return(data)
}
plot_age_pyramid <- function(countryCode,filterYear){
  df <- get_age_pyramid_data(countryCode)
  req(df)
  breaks <- c(seq(0, 100, by = 5),Inf)
  labels <- c(paste(seq(0,95,by=5),seq(5,100,by=5),sep="-"),"100+")
  df <- df %>% 
    filter(Year == filterYear) %>%
    select(-Year) %>%
    mutate(age_group = cut(Age,
                           breaks = breaks,
                           right = F,
                           labels = labels)) %>%
    mutate(age_group = factor(age_group, levels = labels)) %>%
    group_by(age_group,Sex) %>%
    summarise(count = sum(Population)) %>%
    right_join(expand.grid(Sex = c("Male","Female"),
                           age_group = factor(labels,levels = labels)),
               by = c("Sex","age_group")) %>%
    replace_na(list(count=0))
  
  
  
  popplot<- ggplot(df, 
                   aes(
                     x = age_group,
                     fill = Sex, 
                     y = ifelse(
                       test = Sex == "Male", 
                       yes = -count, 
                       no = count
                     )
                   )
  ) + 
    scale_y_continuous(
      labels = abs, 
      limits = max(df$count) * c(-1,1)
    ) +
    coord_flip() +
    geom_bar(stat = "identity") +
    theme_minimal() + 
    labs(
      x = " Group",
      y = "Population Size",
      fill = "Sex",
      title = "Population Pyramid"
    ) + 
    theme(plot.title = element_text(hjust = 0.5))
  return(popplot)
}
getCountryCode <- function(countryName){
  Countries <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Czechia","Denmark",
                 "Finland","France","Hungary","Iceland","Ireland","Italy","Japan",
                 "Netherlands","New Zealand","Norway","Portugal",
                 "Slovakia","Spain","Sweden","Switzerland","U.K.","U.S.A.")
  Codes <-   Codes <- c("AUS","AUT","BLR","BEL","BGR","CAN","CHL","CZE", "DNK",  "FIN","FRATNP",  "HUN",
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
OlisPopSim <- function(populationsize,birthprob,deathprob){
  birthcount <- 0
  deathcount <- 0
  for(i in 1:populationsize){
    udeath <- runif(1,0,1)
    if(udeath<deathprob){deathcount <- deathcount+1}
    ubirth <- runif(1,0,1)
    if(ubirth<birthprob){birthcount <- birthcount+1}
  }
  newpopulationsize <- populationsize+birthcount-deathcount
  return(list("Pop"=newpopulationsize,"Births"=birthcount,"Deaths"=deathcount))
}
pal <- colorNumeric(palette = "Spectral", domain = TotalPopulationData$Total,n=15,reverse=TRUE)


# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "Human Mortality Database",
  bg = "#0062cc",
  underline = TRUE,
  nav_panel(title = "Welcome", 
            h1("Welcome to the welcome page"),
            h2("Functionality:"),
            tags$ul(
              tags$li("Hello World1"),
              tags$li("Hello World2")
            ),
            div(style = "position: absolute; bottom:0;",
                p("HMD. Human Mortality Database. Max Planck Institute for Demographic Research (Germany), University of California, Berkeley (USA), and French Institute for Demographic Studies (France). 
                       Available ", a(href = "https://www.mortality.org","here"), str_glue("(data downloaded on {date}).", date = Sys.Date())
                ))),
  nav_panel(title = "Interactive Map",
            p(fluidRow(
              column(6,leafletOutput("map")),
              column(6,plotOutput("pyramid"),
                     textOutput("prompttext"))),
              sliderInput("year","Year:",value=2000,min=1950,max=2018,step=1,sep="",width="100%"),
              textOutput("country_name"),
              textOutput("description")),
            fluidRow(
              column(6,plotOutput("pyramid"),
                     textOutput("prompttext")),
              column(6,textOutput("description")))),
  nav_panel(title = "Simulation", 
            p(sidebarLayout(
              sidebarPanel(
                numericInput("popsize","Population Size",10000,min=0,max=100000000),
                numericInput("birthprob","Probability of Birth:",0.30,min=0,max=1),
                numericInput("deathprob","Probability of Death:",0.30,min=0,max=1),
                actionButton("generatepop","Generate")
              ),
              mainPanel(
                textOutput("popsim")
              )
            ))
  ))


server <- function(input, output) {
  data.sets <- reactive({
    input_valueA <- input$country[1]
    input_valueB <- input$country[2]
    result <- country_search(input_valueA,input_valueB)
    return(result)
  })
  

  output$map <- renderLeaflet({
    leaflet(data=yearpopdata()$Total) %>%
      addTiles() %>%
      addPolygons(data=world,color = c("green"), weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  layerId = c("U.S.A.","U.K.","Switzerland","Sweden","Spain","Slovakia",
                              "Portugal","Norway","New Zealand","Netherlands","Japan","Italy",
                              "Ireland","Iceland","Hungary","France","Finland","Denmark",
                              "Czechia","Chile","Canada","Bulgaria","Belgium","Belarus","Austria","Australia"),
                  label=round(yearpopdata()$Total),
                  fillColor = pal(yearpopdata()$Total)) # for some reason australia follows us for a bit? need to fix
  })
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    country_name <- click$id  # Extract the clicked country name
    output$country_name <- renderText({
      paste("You clicked on:", country_name)})})
  
  reactivepyramidctry <- reactive({click <- input$map_shape_click
  country_name <- click$id
  result <- getCountryCode(country_name)
  return(result)})
  
  output$pyramid <- renderPlot({req(input$map_shape_click)
    
    plot_age_pyramid(reactivepyramidctry(),input$year)})
  
  output$prompttext <- renderText({req(!isTruthy(input$map_shape_click))
    return("Please Click on a Country")})
  
  output$description <- renderText({req(input$map_shape_click)
    click <- input$map_shape_click
    country_name <- click$id
    description(test_df,input$year,country_name)})
  
  observeEvent(input$generatepop, output$popsim <- renderText(OlisPopSim(input$popsize,input$birthprob,input$deathprob)$Pop))
  
  yearpopdata <- reactive({yearpopdata <- filter(TotalPopulationData,Year==input$year)})
  
}






# Run the application 
shinyApp(ui = ui, server = server)