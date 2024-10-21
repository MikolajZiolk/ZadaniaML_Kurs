#Cwiczenie2
library(tidymodels) 
library(skimr) 
library(GGally) 
library(openair) 
tidymodels_prefer()


air <- mydata |> selectByDate(year = 2002) 
air |> skim()
str(air)

air <- air |> na.omit() #pominięcie pusty danych



