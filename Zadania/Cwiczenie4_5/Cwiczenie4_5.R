#Cwiczenie3 na podstawie 2
library(tidymodels) 
library(skimr) 
library(GGally)
library(ggpubr)
library(dplyr)
library(openair) 
library(DT)
library(ranger)
library(vip)
library(glmnet)
library(readr)
library(ggthemes)
library(rpart.plot) 
tidymodels_prefer()

# Zadanie 4
args(decision_tree)

# Zadanie 5
#Zoptymalizuj hiper-parametry w modelu lasu losowego.
#Dostosuj ilość współczynników w siatce hiper-parametrów.

#przygotowanie danych
air <- mydata |> selectByDate(year = 2002) |> na.omit()
air |> skim()

air |>    
  ggplot(aes(date, o3)) +     
  geom_line() +     
  theme_bw()  

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
data_split <-initial_split(air, prop = 0.75, strata = ozone)
train_data <- training(data_split)
test_data <- testing(data_split)


#rsmaple - CV folds, bootstrap
set.seed(345)
cv_folds <- vfold_cv(data = train_data, v =10)
cv_folds_r5 <- vfold_cv(data=train_data, v=10, repeats = 5)
bootstrap_folds <- bootstraps(data=train_data, times=5)


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
################################
#Dostrojenie hiper-parametrów lasu losowego
#tworzę specyfikacje modelu, która określa, które hiper-parametry dostroić
#########################
# zdefiniowanie parametrów lasu losowaego do tune
tune_spec <-
  rand_forest(mtry = tune(),
              min_n = tune(),
              trees = 1000) |>
  set_engine(engine = "ranger",
             num.threads=parallel::detectCores() - 1) |>
  set_mode(mode = "classification")

tune_spec
extract_parameter_set_dials(tune_spec)

reg_grid  <- grid_regular(
  mtry(range=c(1,10)),
  min_n(),
  levels=5)

reg_grid

### Tune Workflow
tune_work  <- 
  workflow() |> 
  add_model(tune_spec) |> 
  add_recipe(oz_rec)

### dopasowanie modelu tunning
set.seed(345)
ex_metrics  <- 
  yardstick::metric_set(
    accuracy,
    mcc,
    npv,
    roc_auc
  )

rf_tune_fit <- 
  tune_work |> 
  tune_grid(resamples = cv_folds, 
            grid = reg_grid, 
            control = control_grid(save_pred = T),
            metrics = ex_metrics)
rf_tune_fit
#parsnip - model, rf, bez tuningu
rf_mod <-
  rand_forest() |> 
  set_engine(engine = "ranger",
             num.threads = cores - 1) |> 
  set_mode("classification")

rf_workflow <-
  workflow() |> 
  add_model(rf_mod) |> 
  add_recipe(oz_rec)
########################################################
###################modele bez resample##################
#modele bez resample
lr_mod <-
  logistic_reg() |> 
  set_engine("glm")

#workflow
oz_work <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(oz_rec)

oz_fit <-
  oz_work |> 
  fit(data =train_data)

oz_rec |> summary()

oz_fit |> 
  extract_fit_parsnip() |> 
  tidy() |> 
  mutate(coef_stars = signif_stars(p.value)) #istotność

#przewidywanie bez resample
predict(oz_fit, test_data)
predict(oz_fit, test_data, type="prob")

#połączenie oz_fit z test_data
pred_test <- 
  augment(oz_fit, test_data) |>
  select(-ws,
         -wd,
         -nox,
         -o3,
         -nox,
         -no2,
         -pm10,
         -pm25,
         -so2,
         -co,
         -date)
pred_test

#Krzywa ROC
pred_test |> 
  roc_curve(truth = ozone, .pred_Niskie) |> 
  autoplot()

pred_test |> 
  roc_auc(truth = ozone, .pred_Niskie)
#########################################################
#####################RESAMPLE _CV_FOLDS##################

#tune rf
set.seed(456)
rf_fit_rs <-
  rf_workflow |> 
  fit_resamples(cv_folds)

rf_fit_bootstrap <- 
  rf_workflow |>
  fit_resamples(bootstrap_folds) 

rf_fit_r5 <- 
  rf_workflow |> 
  fit_resamples(cv_folds_r5)

# tune lr
lr_fit_rs <-
  oz_work |> 
  fit_resamples(cv_folds)

lr_fit_bootstrap <- 
  oz_work |>
  fit_resamples(bootstrap_folds) 

lr_fit_r5 <- 
  oz_work |> 
  fit_resamples(cv_folds_r5)

#rozdzxielenie wartosci
rf_fit_rs |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

#metryki bez resample
metrics_no_resample <- bind_rows(
  pred_test |>
    roc_auc(truth = ozone, .pred_Niskie),
  
  pred_test |>
    accuracy(truth = ozone, .pred_class)
) |>
  mutate(.approach = "no resampling")

#metryki
#lr
metrics_lr_cv_folds <- lr_fit_rs |> 
  collect_metrics() |> 
  filter(.metric == "accuracy" | .metric == "roc_auc") |>
  mutate(.approach = "lr_cv_folds")

metrics_lr_vcv_r5 <- lr_fit_r5|> 
  collect_metrics() |> 
  filter(.metric == "accuracy" | .metric == "roc_auc") |>
  mutate(.approach = "lr_cv_r5")

metrics_lr_bootstrap <- lr_fit_bootstrap|> 
  collect_metrics() |> 
  filter(.metric == "accuracy" | .metric == "roc_auc") |>
  mutate(.approach = "lr_bootstrap")

#rf
metrics_rf_cv_folds <- rf_fit_rs|> 
  collect_metrics() |> 
  filter(.metric == "accuracy" | .metric == "roc_auc") |>
  mutate(.approach = "rf_cv_folds")

metrics_rf_vcv_r5 <- rf_fit_r5|> 
  collect_metrics() |> 
  filter(.metric == "accuracy" | .metric == "roc_auc") |>
  mutate(.approach = "rf_cv_r5")

metrics_rf_bootstrap <- rf_fit_bootstrap|> 
  collect_metrics() |> 
  filter(.metric == "accuracy" | .metric == "roc_auc") |>
  mutate(.approach = "rf_bootstrap")


all_metrics <- bind_rows(
  metrics_no_resample,
  metrics_lr_cv_folds,
  metrics_lr_vcv_r5,
  metrics_lr_bootstrap,
  metrics_rf_cv_folds,
  metrics_rf_vcv_r5,
  metrics_rf_bootstrap
)
################################################
