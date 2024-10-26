#Cwiczenie2
library(tidymodels) 
library(skimr) 
library(GGally)
library(ggpubr)
library(openair) 
tidymodels_prefer()


air <- mydata |> selectByDate(year = 2002) |> na.omit()
air |> skim()
str(air)

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

# Podgląd zakresu wartości ozonu
air |> 
  pull(o3) |> 
  range()  

# Tworzenie zmiennej jakościowej 'ozone' 
air <-
  air |>
  mutate(ozone = cut(
    o3,
    breaks = c(-0.1, 10, 53),
    labels = c("Niskie", "Wysokie")
  ))

air |> count(ozone)

#podział na zbiór trenigowy i testowy
set.seed(222)
data_split <- initial_split(air, prop = 0.75, strata = ozone)
train_data <- training(data_split)
test_data <- testing(data_split)

# Tworzenie receptury
oz_rec <- 
  recipe(ozone ~ ., data = train_data) |>
  update_role(o3, wd, date, pm10, pm25, so2, co, no2, new_role = "ID") |> 
  step_BoxCox(ws, nox, no2) |>
  step_date(date, features = c("month")) |> #kolumna jakościowa
  step_time(date, features = c("hour")) |> 
  step_mutate(date_hour = as.factor(date_hour)) |>  
  step_dummy(all_nominal_predictors()) |>  # Tworzenie zmiennych fikcyjnych
  step_zv() #usunie kolumny z danych, gdy dane zestawu szkoleniowego mają pojedynczą wartość,

oz_rec |> summary()

oz_rec |>  ##??????
  prep()

lr_mod <-
  logistic_reg() |> 
  set_engine("glm")

logi_work <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(oz_rec)

logi_work

logi_fit <-
  logi_work |> 
  fit(data =train_data)

oz_rec |> summary()

logi_fit |> 
  extract_fit_parsnip() |> 
  extract_recipe() |> 
    tidy()


