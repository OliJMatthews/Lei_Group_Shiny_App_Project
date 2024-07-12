library(shiny)
library(tidyverse)
library(HMDHFDplus)

source(country_search)

# Reads UK Births and Deaths
UKBirths <- readHMDweb("GBR_NP","Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
UKDeaths <- readHMDweb("GBR_NP","Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!")
AustriaBirths <- readHMDweb("AUT","Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
AustriaDeaths <- readHMDweb("AUT","Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!")
DenmarkBirths <- readHMDweb("DNK","Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
DenmarkDeaths <- readHMDweb("DNK","Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!")
IrelandBirths <- readHMDweb("IRL","Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
IrelandDeaths <- readHMDweb("IRL","Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!")
NorwayBirths <- readHMDweb("NOR","Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
NorwayDeaths <- readHMDweb("NOR","Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
SwitzerlandBirths <- readHMDweb("CHE","Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")
SwitzerlandDeaths <- readHMDweb("CHE","Births","om119@leicester.ac.uk","LeicesterShinyProject2024!")


# Adding Country, Pivoting Longer to add Sex Column and removing Total
UKBirths <- UKBirths %>% 
  mutate(Country="UK") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL)
UKDeaths <- UKDeaths %>% 
  mutate(Country="UK") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL,Age=NULL)


AustriaBirths <- AustriaBirths %>% 
  mutate(Country="Austria") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL)
AustriaDeaths <- AustriaDeaths %>% 
  mutate(Country="Austria") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL,Age=NULL)


DenmarkBirths <- DenmarkBirths %>% 
  mutate(Country="Denmark") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL)
DenmarkDeaths <- DenmarkDeaths %>% 
  mutate(Country="Denmark") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL,Age=NULL)

IrelandBirths <- IrelandBirths %>% 
  mutate(Country="Ireland") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL)
IrelandDeaths <- IrelandDeaths %>% 
  mutate(Country="Ireland") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL,Age=NULL)


NorwayBirths <- NorwayBirths %>% 
  mutate(Country="Norway") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL)
NorwayDeaths <- NorwayDeaths %>% 
  mutate(Country="Norway") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL,Age=NULL)

SwitzerlandBirths <- SwitzerlandBirths %>% 
  mutate(Country="Switzerland") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL)
SwitzerlandDeaths <- SwitzerlandDeaths %>% 
  mutate(Country="Switzerland") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL,Age=NULL)

Births <- bind_rows(AustriaBirths,UKBirths,DenmarkBirths,IrelandBirths,NorwayBirths,SwitzerlandBirths)
Deaths <- bind_rows(AustriaDeaths,UKDeaths,DenmarkDeaths,IrelandDeaths,NorwayDeaths,SwitzerlandDeaths)

Deaths <- Deaths %>% group_by(Year,Country,Sex) %>% 
  mutate(value=sum(value)) %>% 
  distinct()

ui <- fluidPage(
    titlePanel("Human Mortality Database"),
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("sex","Sex:",c("Male","Female")),
          selectInput("country","Country (select 2):",c("UK","Austria","Denmark","Ireland","Norway","Switzerland"),multiple=TRUE)
        ),
        mainPanel(
           column(6,textOutput("countryA"),plotOutput("chartB1"),
           plotOutput("chartD1")),
           column(6,textOutput("countryB"),plotOutput("chartB2"), plotOutput("chartD2"))
        )
    )
)

server <- function(input, output) {
BirthsexA<- reactive({BirthSexA <- filter(Births,Sex==input$sex[1]|Sex==input$sex[2],Country==input$country[1])})
DeathsexA <- reactive({DeathSexA <- filter(Deaths,Sex==input$sex[1]|Sex==input$sex[2],Country==input$country[1])})

BirthsexB<- reactive({BirthSexB <- filter(Births,Sex==input$sex[1]|Sex==input$sex[2],Country==input$country[2])})
DeathsexB <- reactive({DeathSexB <- filter(Deaths,Sex==input$sex[1]|Sex==input$sex[2],Country==input$country[2])})

output$countryA <- renderText(input$country[1])
output$countryB <- renderText(input$country[2])
    
# UK Births and Deaths
output$chartB1 <- renderPlot(
  ggplot(BirthsexA() ,aes(x=Year,y=value,color=Sex))+
      geom_point()+
    geom_smooth(se=F)+
    labs(title="Births over Time",ylab=""))

output$chartD1 <- renderPlot(
  ggplot(DeathsexA() ,aes(x=Year,y=value,color=Sex))+
    geom_point()+
    geom_smooth(se=F)+
    labs(title="Deaths over Time",ylab=""))

output$chartB2 <- renderPlot(
  ggplot(BirthsexB() ,aes(x=Year,y=value,color=Sex))+
    geom_point()+
    geom_smooth(se=F)+
    labs(title="Births over Time",ylab=""))

output$chartD2 <- renderPlot(
  ggplot(DeathsexB() ,aes(x=Year,y=value,color=Sex))+
    geom_point()+
    geom_smooth(se=F)+
    labs(title="Deaths over Time",ylab=""))
} 



shinyApp(ui = ui, server = server)
