library(ggplot2)
library(dplyr)
library(shiny)
# Test data
data <- data.frame(
  Age = sample(0:100, 100, replace = T),
  Sex = rep(c("Male","Female"),50),
  Population = round(rnorm(100,100,10))
)


plot_age_pyramid <- function(df){
  # Grouping data-frame into age groups
  req(data)
  breaks <- c(seq(0, 100, by = 5),Inf)
  labels <- c(paste(seq(0,95,by=5),seq(5,100,by=5),sep="-"),"100+")
  df <- df %>% 
    mutate(age_group = cut(Age,
                           breaks = breaks,
                           right = F,
                           labels = labels))%>%
    mutate(age_group = factor(age_group, levels = labels)) %>%
    group_by(age_group,Sex) %>%
    summarise(count = sum(Population)) %>%
    right_join(expand.grid(Sex = c("Male","Female"),
                           age_group = factor(labels,levels = labels)),
               by = c("Sex","age_group")) %>%
    replace_na(list(count=0))
  
  
  
  ggplot(df, 
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
  
}

plot_age_pyramid(data)
