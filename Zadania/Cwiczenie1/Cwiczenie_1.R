#zadanie1
library(tidyverse)
library(GGally)
library(broom)

colnames(airquality) <- tolower(colnames(airquality)) #zmiana nazwy kolumn na małe litery

air <-
  airquality |>
  as_tibble() |> #format wizualizacji tibble
  na.omit() |> #usuniecie wierszy z brakami danych
  select(-day) |> #usuniecie kolumny day
  mutate(month = factor(month)) #konwersja na typ faktor

View(air) # wyświetlenie tabeli

glimpse(air) #struktura danych

GGally::ggpairs(air, aes(color = month)) #Sprawdzanie korelacji między zmiennymi

model <- lm(ozone ~ solar.r + wind + temp + month, data = air) #model regresji liniowej bez uwzględnienia  interakcji między zmiennymi
summary(model) #podsiumowanie modelu

model_tidy <- tidy(model, conf.int = TRUE) #konwersja wyniku modelu na czytelny format


