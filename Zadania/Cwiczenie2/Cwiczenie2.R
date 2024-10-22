#Cwiczenie2
library(tidymodels) 
library(skimr) 
library(GGally)
library(ggpubr)
library(openair) 
tidymodels_prefer()


air <- mydata |> selectByDate(year = 2002) 
air |> skim()
str(air)

air <- air |> na.omit() #pominięcie pusty danych

set.seed(222)
air[sample(1:nrow(air), size = 300, replace = F),] |> 
  select(nox, no2) |> 
  ggpairs()


# wykres regresji liniowej, do sprawdzenia danych 
set.seed(222)
air[sample(1:nrow(air), size = 300, replace = F),] |> 
  select(nox, no2) |> 
  ggplot(aes(nox, no2)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, formula = y ~ x) + 
  stat_cor(label.x = 10, label.y = 80) + 
  stat_regline_equation(label.x = 10, label.y = 82) +
  theme_bw()


air |>    
  ggplot(aes(date, o3)) +     
  geom_line() +     
  theme_bw()

