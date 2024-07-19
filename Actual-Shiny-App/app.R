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
Rates <- read_csv("Rates.csv", col_types = cols(...1 = col_skip()))
Rates$Year <- as.integer(Rates$Year)
Shiny_App_Descriptions <- read_csv("Shiny App Descriptions.csv")
Country <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Czechia","Denmark",
             "Finland","France","Hungary","Iceland","Ireland","Italy","Japan",
             "Netherlands","New Zealand","Norway","Portugal",
             "Slovakia","Spain","Sweden","Switzerland","U.K.","U.S.A.")
world <- ne_countries(scale = "medium", returnclass = "sf",country=append(Country,c("United Kingdom","United States of America")))
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
  a <- df %>% 
    filter(Year==year) %>% 
    filter(Countries==country)
  return(a$Descriptions)
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
              column(6,radioButtons("longchoice","",choices = c("Births","Deaths","Population","Birth Rate","Death Rate")),
                     plotOutput("createlongplot"))),
              sliderInput("year","Year:",value=2000,min=1950,max=2018,step=1,sep="",width="100%"),
              textOutput("country_name"),),
            fluidRow(
              column(6,plotOutput("pyramid"),
                     textOutput("prompttext")),
              column(6,textOutput("description"))),),
  nav_panel(title = "Comparison", 
            p(sidebarLayout(
              sidebarPanel(
                selectInput("countrycomp","Pick two countries to compare:",Country,multiple=TRUE),
                radioButtons("comparisonchoice","",choices = c("Births","Deaths","Population","Birth Rate","Death Rate"))
              ),
              mainPanel(
                plotOutput("comparisonplot")
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
    leaflet() %>%
      addTiles() %>%
      addPolygons(data=world,color = c("green"), weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  layerId = c("U.S.A.","U.K.","Switzerland","Sweden","Spain","Slovakia",
                              "Portugal","Norway","New Zealand","Netherlands","Japan","Italy",
                              "Ireland","Iceland","Hungary","France","Finland","Denmark",
                              "Czechia","Chile","Canada","Bulgaria","Belgium","Belarus","Austria","Australia"))
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
    description(Shiny_App_Descriptions,input$year,country_name)})
  
  output$createlongplot <- renderPlot({
    req(input$map_shape_click)
      click <- input$map_shape_click
      country_name <- click$id
      ratesadjusted <- filter(Rates,Country==country_name) 
      if(input$longchoice=="Births"){
      ggplot(ratesadjusted)+
        geom_line(aes(x=Year,y=Birth_Count,color=Type))+
           scale_color_manual(values=c("coral2", "cornflowerblue", "black"))}
      else if(input$longchoice=="Deaths"){
        ggplot(ratesadjusted)+
          geom_line(aes(x=Year,y=Death_Count,color=Type))+
          scale_color_manual(values=c("coral2", "cornflowerblue", "black"))}
      else if(input$longchoice=="Population"){
        ggplot(ratesadjusted)+
          geom_line(aes(x=Year,y=Pop_Count,color=Type))+
          scale_color_manual(values=c("coral2", "cornflowerblue", "black"))}
      else if(input$longchoice=="Birth Rate"){
        ggplot(ratesadjusted)+
          geom_line(aes(x=Year,y=Birth_Rate,color=Type))+
          scale_color_manual(values=c("coral2", "cornflowerblue", "black"))}
      else if(input$longchoice=="Death Rate"){
        ggplot(ratesadjusted)+
          geom_line(aes(x=Year,y=Death_Rate,color=Type))+
          scale_color_manual(values=c("coral2", "cornflowerblue", "black"))}
      })

  output$comparisonplot <- renderPlot({
    req(input$countrycomp)
    ratescomp <- Rates %>% 
      filter(Country==input$countrycomp) %>% 
      filter(Type=="Total")
    if(input$comparisonchoice=="Births"){
      ggplot(ratescomp)+
        geom_line(aes(x=Year,y=Birth_Count,color=Country))+
        scale_color_manual(values=c("coral2", "cornflowerblue"))}
    else if(input$comparisonchoice=="Deaths"){
      ggplot(ratescomp)+
        geom_line(aes(x=Year,y=Death_Count,color=Country))+
        scale_color_manual(values=c("coral2", "cornflowerblue"))}
    else if(input$comparisonchoice=="Population"){
      ggplot(ratescomp)+
        geom_line(aes(x=Year,y=Pop_Count,color=Country))+
        scale_color_manual(values=c("coral2", "cornflowerblue"))}
    else if(input$comparisonchoice=="Birth Rate"){
      ggplot(ratescomp)+
        geom_line(aes(x=Year,y=Birth_Rate,color=Country))+
        scale_color_manual(values=c("coral2", "cornflowerblue"))}
    else if(input$comparisonchoice=="Death Rate"){
      ggplot(ratescomp)+
        geom_line(aes(x=Year,y=Death_Rate,color=Country))+
        scale_color_manual(values=c("coral2", "cornflowerblue"))}
  })
  
}






# Run the application 
shinyApp(ui = ui, server = server)