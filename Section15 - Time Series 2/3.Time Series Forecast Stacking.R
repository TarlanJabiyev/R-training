# Import libraries & dataset ----
library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)

df <- walmart_sales_weekly %>%
  filter(id == "1_1") %>%
  select(Date, Weekly_Sales)

df %>%
  plot_time_series(
    Date, Weekly_Sales, .smooth_period = "3 months", 
    .interactive = T, .plotly_slider = T)


# Seasonality Evaluation ----
df %>%
  plot_seasonal_diagnostics(
    Date, Weekly_Sales,
    .feature_set = c("week", "month.lbl"),
    .interactive = T)


# Splitting ----
train <- df %>% filter(Date < "2012-08-01")
test <- df %>% filter(Date >= "2012-08-01")


# Feature Engineering ----
recipe_spec <- recipe(Weekly_Sales ~ Date, df) %>%
  step_timeseries_signature(Date) %>%
  step_rm(matches("(iso$)|(xts$)|(day)|(hour)|(min)|(sec)|(am.pm)")) %>%
  step_mutate(Date_week = factor(Date_week, ordered = T)) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(contains("index.num"), Date_year)

recipe_spec %>% prep() %>% juice()


# Make Sub-Models ----

# Auto ARIMA
model_fit_arima <- arima_reg(seasonal_period = 52) %>%
  set_engine("auto_arima") %>%
  fit(Weekly_Sales ~ Date, train)

# Elastic Net
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

wflw_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(train)

# XGBoost
model_spec_xgboost <- boost_tree() %>%
  set_engine("xgboost")

set.seed(123)
wflw_fit_xgboost <- workflow() %>%
  add_model(model_spec_xgboost) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(train)

# NNETAR
model_spec_nnetar <- nnetar_reg(
  seasonal_period = 52,
  non_seasonal_ar = 4,
  seasonal_ar = 1) %>%
  set_engine("nnetar")

set.seed(123)
wflw_fit_nnetar <- workflow() %>%
  add_model(model_spec_nnetar) %>%
  add_recipe(recipe_spec) %>%
  fit(train)

# Prophet w/ Regressors
model_spec_prophet <- prophet_reg(
  seasonality_yearly = T) %>%
  set_engine("prophet") 

wflw_fit_prophet <- workflow() %>%
  add_model(model_spec_prophet) %>%
  add_recipe(recipe_spec) %>%
  fit(train)


# Sub-Model Evaluation ----
submodels_tbl <- modeltime_table(
  model_fit_arima,
  wflw_fit_glmnet,
  wflw_fit_xgboost,
  wflw_fit_nnetar,
  wflw_fit_prophet)

# Accuracy Table
submodels_tbl %>% 
  modeltime_accuracy(test) %>%
  table_modeltime_accuracy(.interactive = F)

# Visualize the forecasts
submodels_tbl %>%
  filter(.model_id == 2) %>% 
  modeltime_forecast(new_data = test,
                     actual_data = df
                     ) %>% 
  plot_modeltime_forecast(.interactive = T)


# Build Modeltime Ensembles ----

# Simple Average Ensemble
ensemble_fit_avg <- submodels_tbl %>%
  ensemble_average(type = "mean")

# Simple Median Ensemble
ensemble_fit_med <- submodels_tbl %>%
  ensemble_average("median")

# Higher Loading on Better Models (Test RMSE)
ensemble_fit_wt <- submodels_tbl %>%
  ensemble_weighted(loadings = c(2, 4, 6, 1, 6))


# Ensemble Evaluation ----
ensemble_models_tbl <- modeltime_table(
  ensemble_fit_avg,
  ensemble_fit_med,
  ensemble_fit_wt)

# Accuracy Table
ensemble_models_tbl %>%
  modeltime_accuracy(test) %>%
  table_modeltime_accuracy(.interactive = F)

# Visualize the performance of the ensembles
ensemble_models_tbl %>%
  filter(.model_id == 1) %>% 
  modeltime_forecast(new_data = test,
                     actual_data = df
                     ) %>%
  plot_modeltime_forecast(.interactive = T)
