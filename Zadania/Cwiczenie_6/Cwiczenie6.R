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

#Podział danych na zbiór treningowy i zbiór testowy
set.seed(222)
data_split <-initial_split(air, prop = 0.75, strata = o3)
train_data <- training(data_split)
test_data <- testing(data_split)

#zbiór walidacyjny i zbiór uczący => bez resamplingu
val_set <-
  validation_split(data = train_data,
                   prop = 3 / 4,
                   strata = o3) 

val_set

#Model regresji liniowej, metodą glmnet
lin_mod <-
  linear_reg(penalty = tune(),
               mixture = 1) |> #???? tune()??
  #mixture = 1 oznacza, że model glmnet potencjalnie usunie nieistotne predyktory i wybierze prostszy model.
  set_engine(engine = "glmnet",
             num.threads = parallel::detectCores() - 1) |>
  set_mode("regression")

# Tworzenie recepturu dla lin_mod
lin_recipe <-    
  recipe(o3 ~ ., data = train_data) |>  
  update_role(date, pm10, pm25, new_role = "ID") |> 
  step_date(date, features = c("month")) |> #kolumna jakościowa
  step_time(date, features = c("hour")) |>    
  step_rm(date) |>    
  step_dummy(all_nominal_predictors()) |>    
  step_zv(all_predictors())

lin_recipe |> 
  prep() |>
  bake(train_data) |> 
  glimpse()





