library(HMDHFDplus)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Might need to change file path Sarah.
# Import and filter out deaths across Males and Females combined.
Rates <- read_csv('~/Lei_Group_Shiny_App_Project/Rates.csv',col_types = cols(...1 = col_skip()))
data <- Rates %>%
  filter(Type != "Total") %>%
  mutate(Type = factor(Type)) %>%
  mutate(Country = factor(Country)) %>%
  mutate(Death_Count = round(Death_Count)) %>%
  mutate(Pop_Count = round(Pop_Count)) %>%
  filter(Year < 2019)


# Plot log mean-variance relationship, under assumption of GLMs Var(Y) = \phi V(\mu) 
# We plot log(Var(Y)) against log(\mu) i.e. log(Var(Y)) = log(\phi) + n*\mu to check which model to use.

mean_variance_data <- data %>%
  group_by(Country,Type) %>%
  summarise(
    mean_val = mean(Death_Count),
    variance = var(Death_Count),
  ) %>%
  mutate(
    log_mean_val = log(mean_val),
    log_variance = log(variance)
  )

lm.out = lm(mean_variance_data$log_variance ~ mean_variance_data$log_mean_val)

ggplot(data = mean_variance_data,aes(x=log_mean_val,y=log_variance)) +
  geom_point() +
  xlim(0,15) +
  ylim(0,25) +
  geom_abline(slope = coef(lm.out)[2],
              intercept = coef(lm.out)[1])

coef(lm.out)
# Slope of 2.008, intercept of -3.683 so Gamma most appropriate as Var(Y) = \phi * \mu^2

death_rate_model <- glm(formula = Death_Count ~ I(Year-min(Year)) + Country + Type + offset(log(Pop_Count)),
                        data = data,
                        family = Gamma(link="log")) 


summary(death_rate_model)


# Similar process for Birth rate model
data <- Rates %>%
  filter(Type != "Total") %>%
  mutate(Type = factor(Type)) %>%
  mutate(Country = factor(Country)) %>%
  mutate(Birth_Count = round(Birth_Count)) %>%
  mutate(Pop_Count = round(Pop_Count)) %>%
  filter(Year < 2019)

mean_variance_data <- data %>%
  group_by(Country,Type) %>%
  summarise(
    mean_val = mean(Birth_Count),
    variance = var(Birth_Count),
  ) %>%
  mutate(
    log_mean_val = log(mean_val),
    log_variance = log(variance)
  )

lm.out = lm(mean_variance_data$log_variance ~ mean_variance_data$log_mean_val)

ggplot(data = mean_variance_data,aes(x=log_mean_val,y=log_variance)) +
  geom_point() +
  xlim(0,15) +
  ylim(0,25) +
  geom_abline(slope = coef(lm.out)[2],
              intercept = coef(lm.out)[1])

summary(lm.out)

# Slope of 1.95 and intercept of -2.856 so Gamma appropriate here again.

birth_rate_model <- glm(formula = Birth_Count ~ I(Year-min(Year)) + Country + Type + offset(log(Pop_Count)),
                        data = data,
                        family = Gamma(link="log")) 

Countries <- c("Australia","Austria","Belarus","Belgium","Bulgaria","Canada","Chile","Czechia","Denmark",
               "Finland","France","Hungary","Iceland","Ireland","Italy","Japan",
               "Netherlands","New Zealand","Norway","Portugal",
               "Slovakia","Spain","Sweden","Switzerland","U.K.","U.S.A.")


years <- rep(2019:2030, each = 2 * length(Countries))
types <- rep(c("Male", "Female"), times = length(2019:2030) * length(Countries))
countries <- rep(Countries, each = 2, times = length(2019:2030))

# Create the data frame
predict_df <- data.frame(
  Year = years,
  Type = types,
  Country = countries,
  Pop_Count = 1000
)


# Create predictions past 2018, predictions seem to be much larger than actual death rates
predict_df <- predict_df %>%
  mutate(pred_data_frame = predict(death_rate_model,predict_df,type = "response")) 


