#Cwiczenie2
library(tidymodels) 
library(skimr) 
library(GGally) 
library(openair) 
tidymodels_prefer()


air <- mydata |> selectByDate(year = 2004) 
air |> skim()
