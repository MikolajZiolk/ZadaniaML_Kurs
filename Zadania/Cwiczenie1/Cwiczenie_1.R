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

GGally::ggpairs(air, aes(color = month)) #Sprawdzanie korelacji między zmiennymi/wizualizacja dancyh

# Skalowanie zmiennych ilościowych
air_scaled <- air |> 
  mutate(across(c(temp, wind, solar.r), scale))  # Skalowanie zmiennych temp, wind, solar.r

head(air_scaled)

model <- lm(ozone ~ solar.r + wind + temp + month, data = air_scaled) #model regresji liniowej bez uwzględnienia  interakcji między zmiennymi
summary(model) #podsiumowanie modelu

model_tidy <- tidy(model, conf.int = TRUE) #konwersja wyniku modelu na czytelny format

predictions <- predict(model, newdata = air_scaled)#predykcja
head(predictions)
