#Cwiczenie 6

library(tidymodels) 
library(skimr) 
library(GGally)
library(ggpubr)
library(dplyr)
library(openair) 
library(DT)
library(ranger)
library(glmnet)
library(readr)
library(ggthemes)
library(purrr)
library(vip)
tidymodels_prefer()

# temat
#przygotowanie danych
air <- mydata |> 
  selectByDate(year = 2002) |> 
  na.omit()

air |> glimpse() #sprawdzenie struktury

#korelacja między zmiennymi
air[sample(1:nrow(air), size = 300, replace = F),] |> 
  ggpairs()
#Występuje istotna korlacja pomiędzy O3 a pozostałymi zmiennymi



# Tworzenie wektora z nazwami kierunków
wind_directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                     "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")


# Przekształcenie `wd` na kierunki wiatru
air <- air |>
  mutate(
    wind_category = cut(
      wd,
      breaks = seq(0, 360, length.out = 17), # 17 punktów dzieli na 16 przedziałów
      labels = wind_directions,
      include.lowest = TRUE
    )
  )

# Sprawdzenie, ile obserwacji jest w każdej kategorii
air |> count(wind_category)








