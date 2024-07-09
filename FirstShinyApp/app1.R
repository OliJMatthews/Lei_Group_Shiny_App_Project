library(shiny)
library(tidyverse)

# Reads UK Births and Deaths
UKBirths <- read.table("Lei_Group_Shiny_App_Project/UKBirths.txt", header=TRUE, quote="\"")
UKDeaths <- read.table("Lei_Group_Shiny_App_Project/UKDeaths.txt", header=TRUE, quote="\"")
AustriaBirths <- read.table("Lei_Group_Shiny_App_Project/AustriaBirths.txt", header=TRUE, quote="\"")
AustriaDeaths <- read.table("Lei_Group_Shiny_App_Project/AustriaDeaths.txt", header=TRUE, quote="\"")

# Adding Country, Pivoting Longer to add Sex Column and removing Total
UKBirths <- UKBirths %>% 
  mutate(Country="UK") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL)
UKDeaths <- UKDeaths %>% 
  mutate(Country="UK") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL)

AustriaBirths <- AustriaBirths %>% 
  mutate(Country="Austria") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL)
AustriaDeaths <- AustriaDeaths %>% 
  mutate(Country="Austria") %>% 
  pivot_longer(c("Male","Female"),names_to="Sex") %>% 
  mutate(Total=NULL)

Births <- bind_rows(AustriaBirths, UKBirths)
Deaths <- bind_rows(AustriaDeaths,UKDeaths)


ui <- fluidPage(
    titlePanel("Human Mortality Database"),
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("sex","Sex:",c("Male","Female")),
          selectInput("country","Country (select 2):",c("UK","Austria"),multiple=TRUE)
        ),
        mainPanel(
           column(6,textOutput("countryA"),plotOutput("chartB1"),
           plotOutput("chartD1")),
           column(6,textOutput("countryB"),plotOutput("chartB2"), plotOutput("chartD2"))
        )
    )
)

server <- function(input, output) {
BirthsexA<- reactive({BirthSexA <- filter(Births,Sex==input$sex,Country==input$country[1])})
DeathsexA <- reactive({DeathSexA <- filter(Deaths,Sex==input$sex,Country==input$country[1])})

BirthsexB<- reactive({BirthSexB <- filter(Births,Sex==input$sex,Country==input$country[2])})
DeathsexB <- reactive({DeathSexB <- filter(Deaths,Sex==input$sex,Country==input$country[2])})

output$countryA <- renderText(input$country[1])
output$countryB <- renderText(input$country[2])
    
# UK Births and Deaths
output$chartB1 <- renderPlot(
  ggplot(BirthsexA() ,aes(x=Year,y=value,fill=Sex))+
      geom_col()+
    labs(title="Births over Time",ylab=""))

output$chartD1 <- renderPlot(
  ggplot(DeathsexA() ,aes(x=Year,y=value,fill=Sex))+
    geom_col()+
    labs(title="Deaths over Time",ylab=""))

output$chartB2 <- renderPlot(
  ggplot(BirthsexB() ,aes(x=Year,y=value,fill=Sex))+
    geom_col()+
    labs(title="Births over Time",ylab=""))

output$chartD2 <- renderPlot(
  ggplot(DeathsexB() ,aes(x=Year,y=value,fill=Sex))+
    geom_col()+
    labs(title="Deaths over Time",ylab=""))
}



shinyApp(ui = ui, server = server)
