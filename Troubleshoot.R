UKBirths <- read.table("/Lei_Group_Shiny_App_Project/UKBirths.txt", header=TRUE, quote="\"")
UKDeaths <- read.table("/Lei_Group_Shiny_App_Project/UKDeaths.txt", header=TRUE, quote="\"")
AustriaBirths <- read.table("/Lei_Group_Shiny_App_Project/AustriaBirths.txt", header=TRUE, quote="\"")
AustriaDeaths <- read.table("/Lei_Group_Shiny_App_Project/AustriaDeaths.txt", header=TRUE, quote="\"")
DenmarkBirths <- read.table("/Lei_Group_Shiny_App_Project/DenmarkBirths.txt", header=TRUE, quote="\"")
DenmarkDeaths <- read.table("/Lei_Group_Shiny_App_Project/DenmarkDeaths.txt", header=TRUE, quote="\"")
IrelandBirths <- read.table("/Lei_Group_Shiny_App_Project/IrelandBirths.txt", header=TRUE, quote="\"")
IrelandDeaths <- read.table("/Lei_Group_Shiny_App_Project/IrelandDeaths.txt", header=TRUE, quote="\"")
NorwayBirths <- read.table("/Lei_Group_Shiny_App_Project/NorwayBirths.txt", header=TRUE, quote="\"")
NorwayDeaths <- read.table("/Lei_Group_Shiny_App_Project/NorwayDeaths.txt", header=TRUE, quote="\"")
SwitzerlandBirths <- read.table("/Lei_Group_Shiny_App_Project/SwitzerlandBirths.txt", header=TRUE, quote="\"")
SwitzerlandDeaths <- read.table("/Lei_Group_Shiny_App_Project/SwitzerlandDeaths.txt", header=TRUE, quote="\"")


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
  summarise(value=sum(value)) %>% 
  filter(Country=="UK")

ggplot(Deaths,aes(x=Year,y=value,color=Sex))+
  geom_point()+
  geom_smooth()
