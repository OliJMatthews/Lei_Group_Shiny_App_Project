library(HMDHFDplus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(MASS)
library(splines)
source("CalculateBirthRates.R")
# Import and filter out deaths across Males and Females combined.
Rates <- read_csv("Actual-Shiny-App/Rates.csv",col_types = cols(...1 = col_skip()))
birth_data <- Rates %>%
  filter(Type == "Total") %>%
  mutate(Country = factor(Country)) %>%
  filter(Year < 2019) %>% 
  filter(Year > 1949) %>%
  mutate(Year = Year - min(Year)) %>%
  select(-Type)

################################################################################

# Birth rate model 
birth_rate_model_gamma <- glm(formula = Birth_Count ~ Year*Country + offset(log(Pop_Count)),
                        data = birth_data,
                        family = Gamma(link="log")) 
summary(birth_rate_model_gamma)
 exp(coef(birth_rate_model_gamma))
# (Intercept) I(Year - min(Year))      CountryAustria      CountryBelarus      CountryBelgium     CountryBulgaria       CountryCanada 
# 0.07829276          0.99295057          0.79205991          0.91414952          0.84114951          0.84629669          1.01612644 
# CountryChile      CountryCzechia      CountryDenmark      CountryFinland       CountryFrance      CountryHungary      CountryIceland 
# 1.16196388          0.83795781          0.87437077          0.96984568          0.78062205          0.81062547          1.07230210 
# CountryIreland        CountryItaly        CountryJapan  CountryNetherlands  CountryNew Zealand       CountryNorway     CountryPortugal 
# 1.13369272          0.96048440          0.86206644          0.98552225          1.14227749          0.89511484          0.99498368 
# CountrySlovakia        CountrySpain       CountrySweden  CountrySwitzerland         CountryU.K.       CountryU.S.A.            TypeMale 
# 1.01311665          0.98169073          0.73652575          0.84288139          0.85009895          1.03486107          1.09888826 

 
birth_rate_model_poisson <- glm(formula = Birth_Count ~ Year + Country + offset(log(Pop_Count)),
                              data = birth_data,
                              family = poisson(link="log")) 
summary(birth_rate_model_poisson)
exp(coef(birth_rate_model_poisson))
# (Intercept) I(Year - min(Year))      CountryAustria      CountryBelarus      CountryBelgium     CountryBulgaria       CountryCanada 
# 0.08713198          0.99249275          0.79614976          0.92087341          0.82125515          0.86300777          0.98621483 
# CountryChile      CountryCzechia      CountryDenmark      CountryFinland       CountryFrance      CountryHungary      CountryIceland 
# 1.17196067          0.84392787          0.85601042          0.95590038          0.73135014          0.82170367          1.06464548 
# CountryIreland        CountryItaly        CountryJapan  CountryNetherlands  CountryNew Zealand       CountryNorway     CountryPortugal 
# 1.13414521          0.95280418          0.86112224          0.95710762          1.13135670          0.87563357          1.00805035 
# CountrySlovakia        CountrySpain       CountrySweden  CountrySwitzerland         CountryU.K.       CountryU.S.A.            TypeMale 
# 1.01738251          0.96880550          0.70389396          0.82947270          0.84034728          1.03025369          1.09772685

# Try also using a Poisson model but with robust standard errors

birth_rate_model_nb <- glm.nb(formula = Birth_Count ~ Year + Country + Type + offset(log(Pop_Count)),
                                data = birth_data, link=log) 
summary(birth_rate_model_nb)
exp(coef(birth_rate_model_nb))
# (Intercept) I(Year - min(Year))      CountryAustria      CountryBelarus      CountryBelgium     CountryBulgaria       CountryCanada 
# 0.07835005          0.99294726          0.79209058          0.91420080          0.84103327          0.84633372          1.01610626 
# CountryChile      CountryCzechia      CountryDenmark      CountryFinland       CountryFrance      CountryHungary      CountryIceland 
# 1.16209413          0.83799484          0.87424494          0.96975854          0.78050112          0.81066249          1.07250324 
# CountryIreland        CountryItaly        CountryJapan  CountryNetherlands  CountryNew Zealand       CountryNorway     CountryPortugal 
# 1.13374038          0.96037755          0.86208532          0.98538845          1.14230007          0.89500116          0.99500224 
# CountrySlovakia        CountrySpain       CountrySweden  CountrySwitzerland         CountryU.K.       CountryU.S.A.            TypeMale 
# 1.01315324          0.98164706          0.73634254          0.84280851          0.85010116          1.03487584          1.09889034 

# Predict for original dataset
predicted<-birth_data[c("Country","Year","Type","Birth_Rate")]
predicted$Pop_Count<-1000
# predicted$Predicted_Birth_Rate<-predict(birth_rate_model_poisson,predicted,type = "response")
predicted$Predicted_Birth_Rate<-predict(birth_rate_model_gamma,predicted,type = "response")
#predicted$Predicted_Birth_Rate<-predict(birth_rate_model_nb,predicted,type = "response")

# Check agreement
plot(predicted$Birth_Rate,predicted$Predicted_Birth_Rate)
abline(0,1,col="red")

# Can check the individual predictions too
with(predicted[predicted$Country=="Australia",], plot(Year,log(Predicted_Birth_Rate),col="red"))
with(predicted[predicted$Country=="Australia",], points(Year,log(Birth_Rate),col="blue",pch=4))

with(predicted[predicted$Country=="U.K.",], plot(Year,log(Predicted_Birth_Rate),col="red"))
with(predicted[predicted$Country=="U.K.",], points(Year,log(Birth_Rate),col="blue",pch=4))

with(predicted[predicted$Country=="Sweden",], plot(Year,log(Predicted_Birth_Rate),col="red"))
with(predicted[predicted$Country=="Sweden",], points(Year,log(Birth_Rate),col="blue",pch=4))

# Could improve fit by:
# Restricting the years for modelling e.g. Sweden has data from 1751 but most other countries have from 1920
# Consider fitting separate models by sex or country (balance of having a large enough sample size and a simpler model)
# Consider including interaction terms between covariates
# Consider modelling year as a non-linear term e.g. fractional polynomials, splines

# Fit a more complex model - the effect of year is not linear
birth_rate_model_poisson_complex <- glm(formula = Birth_Count ~ Year + I(Year^2) + Country +  offset(log(Pop_Count)),
                                data = data,
                                family = poisson(link="log")) 
summary(birth_rate_model_poisson_complex)

predicted$Predicted_Birth_Rate_Complex<-predict(birth_rate_model_poisson_complex,predicted,type = "response")

with(predicted[predicted$Country=="Australia",], plot(Year,log(Predicted_Birth_Rate_Complex),col="red"))
with(predicted[predicted$Country=="Australia",], points(Year,log(Birth_Rate),col="blue",pch=4))

with(predicted[predicted$Country=="U.K.",], plot(Year,log(Predicted_Birth_Rate_Complex),col="red"))
with(predicted[predicted$Country=="U.K.",], points(Year,log(Birth_Rate),col="blue",pch=4))

with(predicted[predicted$Country=="Sweden",], plot(Year,log(Predicted_Birth_Rate_Complex),col="red"))
with(predicted[predicted$Country=="Sweden",], points(Year,log(Birth_Rate),col="blue",pch=4))

# Using splines
birth_rate_model_poisson_splines <- glm(formula = Birth_Count ~ splines::ns(Year,df=5) + Country + offset(log(Pop_Count)),
                                        data = data,
                                        family = poisson(link="log")) 
summary(birth_rate_model_poisson_splines)

predicted$Predicted_Birth_Rate_Splines<-predict(birth_rate_model_poisson_splines,predicted,type = "response")

with(predicted[predicted$Country=="Australia",], plot(Year,log(Predicted_Birth_Rate_Splines),col="red"))
with(predicted[predicted$Country=="Australia",], points(Year,log(Birth_Rate),col="blue",pch=4))

with(predicted[predicted$Country=="U.K.",], plot(Year,log(Predicted_Birth_Rate_Splines),col="red"))
with(predicted[predicted$Country=="U.K.",], points(Year,log(Birth_Rate),col="blue",pch=4))

with(predicted[predicted$Country=="Sweden",], plot(Year,log(Predicted_Birth_Rate_Splines),col="red"))
with(predicted[predicted$Country=="Sweden",], points(Year,log(Birth_Rate),col="blue",pch=4))

# df of 3-5 is usually used but to capture more complex trends more degrees of freedom are needed
# Can also choose the knot locations and place these around the points where the rates change a lot

# Can also include interactions
birth_rate_model_poisson_splines_interactions <- glm(formula = Birth_Count ~ splines::ns(Year,df=15) + Country + offset(log(Pop_Count)),
                                        data = data,
                                        family = poisson(link="log"))
summary(birth_rate_model_poisson_splines_interactions)

predicted$Predicted_Birth_Rate_Splines_Interactions<-predict(birth_rate_model_poisson_splines_interactions,predicted,type = "response")

with(predicted[predicted$Country=="Australia" & predicted$Type=="Female",], plot(Year,log(Predicted_Birth_Rate_Splines_Interactions),col="red"))
with(predicted[predicted$Country=="Australia" & predicted$Type=="Male",], points(Year,log(Predicted_Birth_Rate_Splines_Interactions),col="blue"))
with(predicted[predicted$Country=="Australia" & predicted$Type=="Female",], points(Year,log(Birth_Rate),col="red",pch=4))
with(predicted[predicted$Country=="Australia" & predicted$Type=="Male",], points(Year,log(Birth_Rate),col="blue",pch=4))

# Compared to no interactions
with(predicted[predicted$Country=="Australia" & predicted$Type=="Female",], plot(Year,log(Predicted_Birth_Rate_Splines),col="red"))
with(predicted[predicted$Country=="Australia" & predicted$Type=="Male",], points(Year,log(Predicted_Birth_Rate_Splines),col="blue"))
with(predicted[predicted$Country=="Australia" & predicted$Type=="Female",], points(Year,log(Birth_Rate),col="red",pch=4))
with(predicted[predicted$Country=="Australia" & predicted$Type=="Male",], points(Year,log(Birth_Rate),col="blue",pch=4))

################################################################################

# Mortality rate model
# By only using the total number of deaths and population size it masks the effect of age
# The mortality rate may look to increase but this could just be because the population is getting older
# and the age-specific mortality rates may actually remain the same or decrease
# Could use the age-specific deaths and population sizes and then also include age in the model

getDeathsData <- function(CountryName){
  CountryCode <- getCountryCode(CountryName)
  deathsDF <- readHMDweb(CountryCode,"Deaths_1x1","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
    dplyr::select(-Total,-OpenInterval) %>%
    pivot_longer(c("Female","Male"), names_to = "Sex", values_to = "Death_Count")

popDF <- readHMDweb(CountryCode,"Population","om119@leicester.ac.uk","LeicesterShinyProject2024!") %>%
  dplyr::select(Year, Age, Female2, Male2) %>%
    rename(Female = Female2, Male = Male2) %>%
    pivot_longer(c("Female","Male"), names_to = "Sex", values_to = "Pop_Count")

deaths_data <- deathsDF %>%
  inner_join(popDF, by = c("Year","Sex","Age")) %>%
  mutate(Country = CountryName) %>%
  relocate(Country)

return(deaths_data)

}

# Need to filter as some death counts and Pop Counts are 0, could be fixed but using intervals
deaths_data <- reduce(lapply(Countries,getDeathsData),rbind) %>%
  na.omit() 

deaths_data <- deaths_data %>% 
  filter(Death_Count > 0) %>%
  filter(Pop_Count > 0 ) %>%
  filter(Year > 1949) %>%
  filter(Year < 2019) %>%
  mutate(Year = (Year - 1949)) %>%
  mutate(Sex = factor(Sex))

death_rate_model_gamma <- glm(formula = Death_Count ~ Sex + Year + Country + offset(log(Pop_Count)),
                              data = deaths_data,
                              family = Gamma(link="log")) 
summary(death_rate_model_gamma)
exp(coef(death_rate_model_gamma))

# (Intercept)            SexMale                Age               Year     CountryAustria     CountryBelarus     CountryBelgium 
# 0.0001301373       1.5421406906       1.0847778498       0.9861898772       1.1701977844       1.5377197762       1.1411470112 
# CountryBulgaria      CountryCanada       CountryChile     CountryCzechia     CountryDenmark     CountryFinland      CountryFrance 
# 1.4365722042       0.9785664868       1.1567879489       1.3746970267       1.0991621731       1.1861008580       1.0460377474 
# CountryHungary     CountryIceland     CountryIreland       CountryItaly       CountryJapan CountryNetherlands CountryNew Zealand 
# 1.5024861291       0.9429433935       1.1868720444       1.0445207358       0.9441487748       1.0129734394       1.0680511150 
# CountryNorway    CountryPortugal    CountrySlovakia       CountrySpain      CountrySweden CountrySwitzerland        CountryU.K. 
# 0.9978260038       1.2524483162       1.4213319242       1.0252327356       0.9875682079       0.9813840201       1.1382406136 
# CountryU.S.A. 
# 1.1156864555

predicted<-deaths_data[c("Country","Year","Age","Sex","Death_Count","Pop_Count")]
predicted <- predicted %>%
  mutate(Death_Rate = (Death_Count / Pop_Count * 1000)) %>%
  mutate(Predicted_Death_Rate = predict(death_rate_model_gamma)) %>%
  mutate(Pop_Count = 1000)


# Can check the individual predictions too
with(predicted[predicted$Country=="Australia" & predicted$Age==0 & predicted$Sex=="Male",], plot(Year,log(Predicted_Death_Rate),ylim = c(1,4),col="red"))
with(predicted[predicted$Country=="Australia" & predicted$Age==0 & predicted$Sex=="Female",], points(Year,log(Predicted_Death_Rate),col="blue"))
with(predicted[predicted$Country=="Australia" & predicted$Age==0 & predicted$Sex=="Male",], points(Year,log(Death_Rate),col="red",pch = 4))
with(predicted[predicted$Country=="Australia" & predicted$Age==0 & predicted$Sex=="Female",], points(Year,log(Death_Rate),col="blue",pch = 4))

death_rate_model_gamma <- glm(formula = Death_Count ~ Age*Year*Country + offset(log(Pop_Count)),
                              data = deaths_data,
                              family = Gamma(link="log")) 
summary(death_rate_model_gamma)
exp(coef(death_rate_model_gamma))

summary(death_rate_model_gamma)
exp(coef(death_rate_model_gamma))

################################################################################
final_birth_rate_predictions <- expand.grid(Country = Countries,Year = 2018:2030 - 1949,Pop_Count = 1000) 
final_birth_rate_predictions$Predicted_Birth_Rate <- predict(birth_rate_model_poisson_splines,newdata = final_birth_rate_predictions) 
final_birth_rate_predictions$Year <- final_birth_rate_predictions$Year + 1949

write.csv(final_birth_rate_predictions,"~/Lei_Group_Shiny_App_Project/Birth_Rate_Predictions.csv")


final_death_rate_predictions <- expand.grid(Country = Countries,Age = 0:110,Year = 2018:2030 - 1949,Pop_Count = 1000)
final_birth_rate_predictions$Predicted_Death_Rate <- predict(birth_rate_model_poisson_splines,newdata = final_birth_rate_predictions) 

