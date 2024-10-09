#zadanie1
library(tidyverse)
library(GGally)

colnames(airquality) <- tolower(colnames(airquality)) #zmiana nazwy kolumn na małe litery

air <-
  airquality |>
  as_tibble() |> #format wizualizacji tibble
  na.omit() |> #usuniecie wierszy z brakami danych
  select(-day) |> #usuniecie kolumny day
  mutate(month = factor(month)) #konwersja na typ faktor

View(air) # wyświetlenie tabeli

glimpse(air) 
