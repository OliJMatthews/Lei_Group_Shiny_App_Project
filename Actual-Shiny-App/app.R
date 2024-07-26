library(shiny)
library(tidyverse)
library(bslib)
library(HMDHFDplus)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


predicttest <- data.frame(Country=c("U.K.","U.K.","U.K.","U.K.","U.K.","U.K.","U.K.","U.K.","U.K.","U.K.","U.K.","U.K.","U.K.","U.K.","U.K."),
                          Year=c(2019,2019,2019,2020,2020,2020,2021,2021,2021,2022,2022,2022,2023,2023,2023),
                          Type=c("Male","Female","Total","Male","Female","Total","Male","Female","Total","Male","Female","Total","Male","Female","Total"),
                          Birth_Count=c(50000,60000,70000,60000,70000,80000,70000,80000,90000,50000,50000,50000,50000,50000,50000),
                          Death_Count=c(5000,5000,5000,5000,5000,5000,5000,5000,5000,5000,5000,5000,5000,5000,5000),
                          Pop_Count=c(5000,5000,5000,5000,5000,5000,5000,5000,5000,5000,5000,5000,5000,5000,5000),
                          Birth_Rate=c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5),
                          Death_Rate=c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5)
)
Birth_Rate_Predictions <- read_csv("Birth_Rate_Predictions.csv", 
                                   col_types = cols(...1 = col_skip(), Pop_Count = col_skip()))
Rates <- read_csv("Rates.csv", col_types = cols(...1 = col_skip()))
Rates$Year <- as.integer(Rates$Year)
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
  bg = "#00c5cd",
  underline = TRUE,
  nav_panel(title = "Welcome", 
            h1("Welcome to the welcome page"),
            h2("Functionality:"),
            tags$ul(
              tags$li("Hello World1"),
              tags$li("Hello World2")
            ),
            div(style = "position: absolute; bottom:0;",
                p("HMD. Human Mortality Database. Max Planck Institute for Demographic Research (Germany), University of California, 
                Berkeley (USA), and French Institute for Demographic Studies (France). 
                       Available ", a(href = "https://www.mortality.org","here"), str_glue("(data downloaded on 23/07/2024).")
                ))),
  nav_panel(title="Interactive Map",
            p(fluidRow(
              sidebarPanel(textOutput("prompttext"),
                           leafletOutput("map"),
                           radioButtons("longchoice","",choices = c("Births","Deaths","Population","Birth Rate","Death Rate")),
                           textOutput("country_name")
              ),
              mainPanel(plotOutput("createlongplot"),
                        sliderInput("year","Year:",value=2000,min=1950,max=2018,step=1,sep="",width="100%"),
                        plotOutput("pyramid"))
            ))),
  nav_panel(title = "Comparison", 
            p(sidebarLayout(
              sidebarPanel(
                selectInput("countrycomp","Pick Countries to Compare:",Country,multiple=TRUE),
                radioButtons("comparisonchoice","",choices = c("Births","Deaths","Population","Birth Rate","Death Rate"))
              ),
              mainPanel(
                plotOutput("comparisonplot")
              )
            ))
  ),
  nav_panel(title="Case Study: U.K. vs Japan",
            p("Economic prosperity gives rise to significant lifestyle changes, allowing for the prioritisation of careers and 
            education over family formation. Both the UK and Japan, as high-income countries have lower mortality and birth rates 
            due to factors such as increased educational and career opportunities for women, higher living costs and wide-spread 
            access to healthcare and family planning. While these trends reflect advancements in socio-economic conditions and medical 
            access, they are also indicative of the challenges of population ageing."),
            p(tags$b("Japan") ,"has the highest proportion of elderly citizens in the world, with over 28% of its population aged 65 and older.
            This demographic shift is due to Japan’s sub-replacement fertility rate and decreased mortality rates. Figure 1 provides a visual
            representation of the age distribution across different gender groups in Japan. "),
            fluidRow(column(6,tags$b("1.A."), plotOutput("japan1950",height="300px")),
                     column(6,tags$b("1.B."), plotOutput("japan2018",height="300px"))),
            p(tags$b("Figure 1.A Population Pyramid of Japan (1950) Figure 1.B Population Pyramid of Japan (2018) ")),
            p(tags$b("1.A."), "The population pyramid depicts a broad base which tapers towards the top, indicating that
            there is a high birth rate and lower life expectancy. This shape is reflective of the larger proportion of younger 
            individuals and smaller number of elderly individuals. Males are represented in blue, and females in red, gender
            distribution is generally balanced across most age groups though there are more elderly women compared to men in the 65+ age
            group. "),
            p(tags$b("1.B."), "The population pyramid has a narrower base and a wider top, indicative of the low birth rate and increased proportion 
            of elderly individuals."),
            
            p(tags$b("The UK"),"also is experiencing an ageing population, with around 18% if its population aged 65 and older. The UK also has a
            sub-replacement fertility rate and mortality rates have similarly decreased over time. Figure 2 illustrates the demographic shift towards 
            an ageing population in the UK."),
            fluidRow(column(6,tags$b("2.A."), plotOutput("uk1950",height="300px")),
                     column(6,tags$b("2.B."), plotOutput("uk2018",height="300px"))),
            p(tags$b("Figure 2.A Population Pyramid of UK (1950) Figure 2.B Population Pyramid of UK (2018) ")),
            p(tags$b("2.A."), "The population pyramid for the UK in the 1950 displays a broad base that gradually narrows towards the top, indicating 
          the high birth rate and low life expectancy at the time. The shape is reflective of the higher proportion of younger individuals."),
            p(tags$b("2.B."),"The population pyramid for the UK in 2018 shows a gradual increase in the older population which a moderately narrow base 
            and an expanded middle section. This reflects a lower birth rate and a growing proportion of individuals aged 50-55. "),
            p(tags$b("Sub-Replacement Fertility Rate  ")),
            p("The number of births is lower than the number necessary to maintain population size – this is attributed to many socio-economic factors."),
            p("In Japan, Increased cost of living has disincentivised having children as expenses related to housing and education are substantial. There is 
            also a cultural expectation to have children only after marriage which incurs another set of costs – this is reflected in the fact that only 
            2.2% of Japanese children are born outside of marriage. Furthermore, the poor work-life balance as a result of long working hours and overtime 
            expectations leaves very little time for family life."),
            p("In the UK, childcare costs are prohibitively high – this in conjunction with higher educational attainment and career ambition have led to the 
            prioritisation of professional growth over childbearing."),
            p(tags$b("Decreased Mortality Rates")),
            p("Japan’s mortality rates have decreased due to advancements in the medical field with vaccine rollouts and excellent health education. The 
            healthcare system places a strong emphasis on preventative care, leading to early detection and treatment of illness. Additionally, there is a 
            cultural belief in civic responsibility regarding health, which promotes better diets and adoption of active lifestyles."),
            p("In the UK, improved living conditions and education – combined with widespread access to healthcare services have led to a rapid decline in 
            mortality rate. "),
            p("Despite these similar demographic shifts, the quality of life for elderly populations in the UK and Japan varies significantly – specifically in 
            the management of non-communicable disease (NCD) which is seen in figure 3."),
            p(tags$b("What are Non-Communicable Diseases? ")),
            p("Non-communicable diseases, known more commonly as chronic diseases are the leading cause of death globally. Examples include heart disease, cancer, 
            chronic respiratory disease and diabetes – their chronic nature requires ongoing medical care, and management. The risk of developing NCDs increases 
            with age, consequently leading to high prevalence amongst older adults."),
            p(tags$b("What is the significance of NCDs in healthcare? ")),
            p(tags$u("Increased Vulnerability ")),
            p("Non-communicable diseases pose significant challenges to healthcare systems due to the increased vulnerability of affected individuals, especially 
            during pandemics. Evidence indicates that NCDs increased the likelihood of hospitalisation or death from COVID-19, highlighting the increased 
            vulnerability of individuals with NCDs and the consequent strain on health services. This exemplifies the risk posed by these conditions, 
            in regards to individual health and to the stability of healthcare systems."),
            p(tags$u("Multimorbidity and its Complications")),
            p("Individuals with NCDs can also suffer from multiple chronic conditions simultaneously, which is referred to as multimorbidity. They may experience 
            chronic pain, poor mental wellbeing and impairment of daily living – affecting their independence."),
            p("Managing NCDs incurs significant healthcare costs, including expenses related to medication, rehabilitation and long-term care as these conditions 
            often require lifelong management which results in continuous healthcare expenditure. There are also personal losses as a result that can impact people 
            on an individual level, absences and poor productivity as a result of illness may impact income and result in financial stress."),
            fluidPage(plotOutput("izanncds",height="300px")),
            p(tags$b("Figure 3: NCD Mortality Rates in Japan and the UK")),
            p("The figure shows the trend in non-communicable disease mortality rates for Japan and the UK from the year 2000 to 2015. Japan is shown in blue while the 
            UK is shown in red. The mortality rate in Japan has been consistently low and shows a decreasing trend over time – this is indicative of effective management 
            and prevention of NCDs. The UK had a significantly high NCD mortality rate in the early 2000s, however with effective healthcare initiatives this has 
            decreased. Despite this decline, the UK’s mortality rates remain significantly higher in comparison to Japan."),
            p(tags$b("Japan’s healthcare initiatives and approach to combat NCDs.")),
            p("Japan, despite having the oldest population in the world – has managed to maintain one of the lowest NCD mortality 
            rates due to its proactive healthcare strategies."),
            p(tags$u("Health and Medical Services Act for the Aged (1983)")),
            p("This act was designed to address the onset of lifestyle related diseases through prevention, aimed at middle aged 
            individuals to reduce the likelihood of chronic conditions as they age."),
            p("Under this act, all citizens over the age of 40 would be provided with a free package that included personal health 
            records, coupons for free routine health check-ups and screenings as well as follow up advice from medical professionals."),
            p("Building on this, the Specific Health Check-ups and Guidance System was implemented in 2008. This system was designed to enforce 
            the preventative scope of the Health and Medical Services Act – and resulted in mandatory annual health check-ups and screenings 
            – including body height, weight, waist circumference and blood pressure in conjunction with blood lipid and glucose levels."),
            p("This, assessed alongside medical history and self-reported lifestyle information would be used to categorise individuals into risk 
            levels from A to D. Following this, individuals are provided with personalised health guidance based on these classifications including 
            dietary adjustments or consultations with other medical professionals. Those in higher risk categories are encouraged to participate in 
            more rigorous programs involving monthly monitoring."),
            p(tags$u("Health Japan 21 ")),
            p("In the 2000s, Japan implemented the Health Japan 21 program. This was a national campaign aimed at addressing the risk factors associated 
            with NCD development – marking a significant shift towards a proactive approach combining individual health as a civic responsibility."),
            p("The program focused on a range of issues including hypertension, smoking, excessive alcohol consumption, hyperlipidaemia which were outlined 
            through the use of epidemiological studies that highlighted the prevalence of stroke and these risk factors as a primary reason for NCD death."),
            p("An evaluation in 2018 assessed the progress and impact of Health Japan 21, some achievements included:",
            tags$div(
              tags$li("Decrease in health inequalities and overall health outcome improvement across different socio-economic groups "),
              tags$li("Increased cancer screening participation"),
              tags$li("Lower suicide rates "),
              tags$li("Improved nutritional standards for corporations "))),
            p(tags$u("Accessibility")),
            p("A key factor in Japan’s success in managing NCDs is how accessible its healthcare system is for low-income groups. Widespread 
              accessibility of ‘Check-up hospitals’, where students and employees are mandated to receive comprehensive health reports at no 
              personal cost – ensures early detection and prevention of NCDs. This is enforced under the School Health and Safety Act and the 
              Industrial Safety and Health Law; companies and schools must provide this and failure to comply may result in fines."),
            p(tags$b("UKs healthcare initiatives and approach to combat NCDs.")),
            p("The UK’s approach to combatting NCD’s is focuses on disincentivising unhealthy lifestyles."),
            p("The Tobacco and Related Products Regulations (2016) enforced plain packaging, and advertising restrictions to reduce smoking 
              rates which is a major risk factor for cancer, cardiovascular disease and various respiratory conditions. This is supported by 
              free local ‘stop smoking’ services, which are easy to access and ensure a good chance of smoking cessation if used in conjunction 
              with nicotine replacement therapy."),
            p("To address childhood obesity and prevent the development of NCDs later in life, the UK introduced the Soft Drinks Industry Levy in 
              2018 – this levy introduced a tax on sugary drinks to reduce excessive sugar consumption among children. Supporting this initiative, 
              the ‘Change4Life’ campaign promotes healthy eating and regular physical activity through resources and activities aimed at families and children."),
            p("The UK emphasises early detection of NCDs through the provision of free optional health checks for individuals aged 40-74. These checks aim to identify 
              early signs of conditions such as heart disease, diabetes, and stroke allowing for timely management."),
            p(tags$b("The UK NHS Long Term Plan")),
            p("The NHS aims to implement early detection services while supporting individuals in becoming actively involved in their own health management. Addressing 
              health inequalities is a core component of the program, recognising that people in the most deprived groups often develop chronic illnesses a decade earlier 
              than those in the least deprived groups. "),
            p(tags$u("Social Determinants of Health and NCDs")),
            p("The social determinants of health (SDH) are non-medical factors that impact health outcomes, these can include economic stability, access to healthcare, 
              built environment and education. These determinants are closely linked to behavioural risk factors for non-communicable diseases (NCDs) such as poor diet, 
              sedentary lifestyles, tobacco smoking and alcohol consumption."),
            p("Obesity is closely interlinked with SDHs, economic constraints limit the ability of lower income individuals to follow recommended dietary guidelines. 
              Households in the bottom income decile would have to spend over 70% of their income in order to follow recommended health guidelines. Balancing costs of 
              utilities, rent and healthy food becomes difficult, forcing many people to choose cheaper and less nutritious options. The accessibility and affordability 
              of fast food, in conjunction with its convenience make it an attractive option for those struggling financially."),
            p("Lower socioeconomic groups are more likely to consume higher levels of alcohol. The psychological stress of deprivation can often lead to poor coping mechanisms 
              such as smoking, excessive alcohol consumption and comfort eating. Countries have seen significant increases in alcohol related mortality during times of economic 
              duress, this is pattern is seen on a microcosmic level in lower socioeconomic groups."),
            p("Addressing these SDHs may be key to reducing deaths from NCDs.")
            
            
            )
            
            
)


server <- function(input, output) {
  output$inc <- renderUI(includeHTML("eg.html"))
  
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
  output$createlongplot <- renderPlot({
    req(input$map_shape_click)
    click <- input$map_shape_click
    country_name <- click$id
    ratesadjusted <- filter(Rates,Country==country_name) %>% 
      filter(Year>=1950) %>% 
      filter(Year<=2018)
    Birth_Rate_Predictions <- filter(Birth_Rate_Predictions,Country==country_name)
    if(input$longchoice=="Births"){
      ggplot(ratesadjusted)+
        geom_line(aes(x=Year,y=Birth_Count,color=Type))+
        scale_color_manual(values=c("coral2", "cornflowerblue", "black"))+
        ylab("Number of Births")+
        ggtitle("Births over Time")+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5))}
    else if(input$longchoice=="Deaths"){
      ggplot(ratesadjusted)+
        theme_minimal()+
        geom_line(aes(x=Year,y=Death_Count,color=Type))+
        scale_color_manual(values=c("coral2", "cornflowerblue", "black"))+
        ylab("Number of Deaths")+
        ggtitle("Deaths over Time")+
        theme(plot.title = element_text(hjust = 0.5))}
    else if(input$longchoice=="Population"){
      ggplot(ratesadjusted)+
        theme_minimal()+
        geom_line(aes(x=Year,y=Pop_Count,color=Type))+
        scale_color_manual(values=c("coral2", "cornflowerblue", "black"))+
        ylab("Population")+
        ggtitle("Population over Time")+
        theme(plot.title = element_text(hjust = 0.5))}
    else if(input$longchoice=="Birth Rate"){
      dataadj <- filter(ratesadjusted,Type=="Total")
      ggplot(dataadj)+
        theme_minimal()+
        geom_line(aes(x=Year,y=Birth_Rate))+
        geom_line(data=Birth_Rate_Predictions,aes(x=Year,y=Predicted_Birth_Rate),linetype="dotted")+
        ylab("Number of Births per 1000 People")+
        ggtitle("Birth Rate over Time")+
        theme(plot.title = element_text(hjust = 0.5))}
    else if(input$longchoice=="Death Rate"){
      ggplot(ratesadjusted)+
        theme_minimal()+
        geom_line(aes(x=Year,y=Death_Rate,color=Type))+
        scale_color_manual(values=c("coral2", "cornflowerblue", "black"))+
        geom_line(data=predicttest,aes(x=Year,y=Death_Rate,color=Type),linetype="dotted")+
        ylab("Number of Deaths per 1000 People")+
        ggtitle("Death Rate over Time")+
        theme(plot.title = element_text(hjust = 0.5))}
  })
  output$comparisonplot <- renderPlot({
    req(input$countrycomp)
    country <- input$countrycomp
    ratescomp <- Rates %>% 
      filter(Country %in% country) %>% 
      filter(Type=="Total") %>% 
      filter(Year>=1950) %>% 
      filter(Year<=2018)
    if(input$comparisonchoice=="Births"){
      ggplot(ratescomp)+
        theme_minimal()+
        geom_line(aes(x=Year,y=Birth_Count,color=Country))+
        ylab("Number of Births")+
        ggtitle("Births over Time")+
        theme(plot.title = element_text(hjust = 0.5))}
    else if(input$comparisonchoice=="Deaths"){
      ggplot(ratescomp)+
        theme_minimal()+
        geom_line(aes(x=Year,y=Death_Count,color=Country))+
        ylab("Number of Deaths")+
        ggtitle("Deaths over Time")+
        theme(plot.title = element_text(hjust = 0.5))}
    else if(input$comparisonchoice=="Population"){
      ggplot(ratescomp)+
        theme_minimal()+
        geom_line(aes(x=Year,y=Pop_Count,color=Country))+
        ylab("Population Size")+
        ggtitle("Population over Time")+
        theme(plot.title = element_text(hjust = 0.5))}
    else if(input$comparisonchoice=="Birth Rate"){
      ggplot(ratescomp)+
        theme_minimal()+
        geom_line(aes(x=Year,y=Birth_Rate,color=Country))+
        ylab("Number of Births per 1000 People")+
        ylim(0,30)+
        ggtitle("Birth Rate over Time")+
        theme(plot.title = element_text(hjust = 0.5))}
    else if(input$comparisonchoice=="Death Rate"){
      ggplot(ratescomp)+
        theme_minimal()+
        geom_line(aes(x=Year,y=Death_Rate,color=Country))+
        ylab("Number of Deaths per 1000 People")+
        ylim(0,20)+
        ggtitle("Death Rate over Time")+
        theme(plot.title = element_text(hjust = 0.5))}
  })
  output$japan1950 <- renderPlot(plot_age_pyramid("JPN",1950))
  output$japan2018 <- renderPlot(plot_age_pyramid("JPN",2018))
  output$uk1950 <- renderPlot(plot_age_pyramid("GBR_NP",1950))
  output$uk2018 <- renderPlot(plot_age_pyramid("GBR_NP",2018))
  output$izanncds <- renderPlot({NCD <- read_csv("NCD.csv", 
                                                 col_types = cols(IndicatorCode = col_skip(), 
                                                                  Indicator = col_skip(), ValueType = col_skip(), 
                                                                  ParentLocationCode = col_skip(), 
                                                                  ParentLocation = col_skip(), `Location type` = col_skip(), 
                                                                  Location = col_skip(), `Period type` = col_skip(), 
                                                                  IsLatestYear = col_skip(), `Dim1 type` = col_skip(), 
                                                                  Dim1 = col_skip(), Dim1ValueCode = col_skip(), 
                                                                  `Dim2 type` = col_skip(), Dim2 = col_skip(), 
                                                                  Dim2ValueCode = col_skip(), `Dim3 type` = col_skip(), 
                                                                  Dim3 = col_skip(), Dim3ValueCode = col_skip(), 
                                                                  DataSourceDimValueCode = col_skip(), 
                                                                  DataSource = col_skip(), FactValueNumericPrefix = col_skip(), 
                                                                  FactValueUoM = col_skip(), FactValueNumericLowPrefix = col_skip(), 
                                                                  FactValueNumericLow = col_skip(), 
                                                                  FactValueNumericHighPrefix = col_skip(), 
                                                                  FactValueNumericHigh = col_skip(), 
                                                                  Value = col_skip(), FactValueTranslationID = col_skip(), 
                                                                  FactComments = col_skip(), Language = col_skip(), 
                                                                  DateModified = col_skip()))
                                
                                ggplot(NCD)+
                                  geom_line(aes(x=Period,y=FactValueNumeric,color=SpatialDimValueCode))+
                                  theme_minimal()+
                                  labs(title = "NCD Mortality Rates over Time")+
                                  xlab("Year")+
                                  ylab("Non-Communicable Disease Rate")
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)