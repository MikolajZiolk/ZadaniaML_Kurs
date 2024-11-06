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

#####################LINEAR REGRESSION###########
#Model regresji liniowej, metodą glmnet
lin_mod <-
  linear_reg(penalty = tune(),
               mixture = tune()) |>
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

# workflow dla lin_mod
lin_workflow <-    
  workflow() |>    
  add_model(lin_mod) |>    
  add_recipe(lin_recipe)

# Siatka optymalizacji hipermaparametrów
# automat 
lin_grid <- grid_regular(penalty(),mixture(), levels = 5)  #5 wartosci kandydujących
lin_grid

# Uczenie i optymalizacja modelu
# Tune model dla linear 
lin_res <-
  lin_workflow |>
  tune_grid(
    resamples = val_set,
    grid = lin_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(mae)
  )

lin_res

#Wykaz kandydatów na najlepszy model
top_lin_models <- 
  lin_res |> 
  show_best(metric="mae", n = Inf) |> 
  arrange(penalty) |> 
  mutate(mean = mean |> round(x = _, digits = 3))

top_lin_models |> gt::gt()

# Wybór najlepszych modeli
lin_best <-
  lin_res |>
  select_best(metric="mae")

lin_best

#model ostateczny
lin_best_mod <-
  lin_workflow |>
  finalize_workflow(lin_best)

lin_fit <-
  lin_best_mod |>
  last_fit(split = data_split)

##########################las losowy, rf=>rand forest##################
# liczba rdzeni na komputerze
cores <- parallel::detectCores()
cores #8 rdzeni

# model rf
rf_mod <-
  rand_forest(mtry = tune(),
              min_n = tune(),
              trees = tune()) |>
  set_engine(engine = "ranger",
             num.threads = parallel::detectCores() - 1,
             importance = "impurity") |>
  set_mode(mode = "regression")

# receptura rf
rf_recipe <- 
  recipe(o3 ~ ., data = train_data) |> 
  update_role(date, pm10, pm25, new_role = "ID") |>
  step_date(date, features = c("month")) |> #kolumna jakościowa
  step_time(date, features = c("hour")) |> 
  step_rm(date) |> 
  step_zv(all_predictors()) 

rf_recipe |>
  prep() |>
  bake(train_data) |>
  glimpse()

rf_workflow <- 
  workflow() |> 
  add_model(rf_mod) |> 
  add_recipe(rf_recipe)

# Siatka regularna  rf
rf_grid <-
  grid_regular(
    mtry(range=c(1, 8)),
    trees(),
    min_n(),
    levels = 5
  )

# tune rf
rf_res <- 
  rf_workflow |> 
  tune_grid(resamples = val_set, 
            grid = rf_grid, 
            control = control_grid(save_pred = T),
            metrics = metric_set(mae))
rf_res

#Wykaz kandydatów na najlepszy model
rf_top_models <-
  rf_res |>
  show_best(metric="mae", n = Inf) |>
  arrange(trees) |>
  mutate(mean = mean |> round(x = _, digits = 3))

rf_top_models |> gt::gt()

rf_res |> show_best(n = 5, metric="mae") #5 najlepszych wyników

# Wybór najlepszego modelu
rf_best <-
  rf_res |>
  select_best(metric="mae")

rf_best

#model ostateczny
rf_best_mod <-
  rf_workflow |>
  finalize_workflow(rf_best)

rf_fit <-
  rf_best_mod |>
  last_fit(split = data_split)

rf_fit |> 
  collect_metrics()

#####################DRZEWO DECYZYJNE#######################
#model decision tree
dec_mod <- 
  decision_tree(
    cost_complexity = tune(), 
    tree_depth = tune(),
    min_n = tune()) |> 
  set_engine("rpart") |> 
  set_mode("regression")
dec_mod

# receptura 
dec_rec <-
  recipe(o3 ~ ., data = train_data) |>
  update_role(date, pm10, pm25, new_role = "ID") |>
  step_date(date, features = c("month")) |> 
  step_time(date, features = c("hour")) |>
  step_rm(date) |> 
  step_zv(all_predictors()) 

dec_rec |>
  prep() |>
  bake(train_data) |>
  glimpse()

#workflow dla dec
dec_workflow <- 
  workflow() |> 
  add_model(dec_mod) |> 
  add_recipe(dec_rec)

#siatka dla dec
dec_grid <- grid_regular(cost_complexity(), 
                       tree_depth(), 
                       min_n(),
                       levels = 5)
dec_grid

#optymalizacja tune
dec_fit_tree <-
  dec_workflow |>
  tune_grid(
    resamples = val_set,
    grid = dec_grid,
    control = control_grid(save_pred = T),
    metrics = metric_set(mae)
  )

dec_fit_tree |> collect_metrics()

dec_fit_tree |> show_best(metric="mae")

dec_top_models <-
  dec_fit_tree |>
  show_best(metric="mae", n = Inf) |>
  arrange(tree_depth) |>
  mutate(mean = mean |> round(x = _, digits = 3))

dec_top_models |> gt::gt()

dec_best <-
  dec_fit_tree |>
  select_best(metric="mae")

dec_best

dec_best_mod <-
  dec_workflow |>
  finalize_workflow(dec_best)

dec_final_fit <-
  dec_best_mod |>
  last_fit(split = data_split)

dec_final_fit |> 
  collect_metrics()

#######################WYKRESY DO MODELI###################
#model linear regression
lin_fit |> 
  extract_fit_parsnip() |> 
  vip(num_features = 20) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_boxplot(color = "black", fill = "grey85") +
  ggdark::dark_theme_dark()

#MODEL Rand forest
rf_fit |> 
  extract_fit_parsnip() |> 
  vip(num_features = 20) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_boxplot(color = "black", fill = "grey85") +
  ggdark::dark_theme_dark()

#model decision tree
dec_final_fit |> 
extract_fit_parsnip() |> 
  vip(num_features = 20) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_boxplot(color = "black", fill = "grey85") +
  ggdark::dark_theme_dark()



# Funkcja do generowania wykresu rozrzutu z linią idealną
plot_scatter_with_ideal <- function(final_fit, model_name) {
  final_fit |> 
    collect_predictions() |> 
    ggplot(aes(x = .pred, y = o3)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = paste("Scatter plot with ideal line for", model_name),
         x = "Predicted O3 Levels",
         y = "Actual O3 Levels") +
    theme_minimal()
}

# Wykresy dla poszczególnych modeli
plot_scatter_with_ideal(lin_fit, "Linear Regression (GLMNet)")
plot_scatter_with_ideal(rf_fit, "Random Forest (Ranger)")
plot_scatter_with_ideal(dec_final_fit, "Decision Tree (RPart)")

