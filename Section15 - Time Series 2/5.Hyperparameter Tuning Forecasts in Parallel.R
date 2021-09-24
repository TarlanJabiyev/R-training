# Import libraries & dataset ----
library(tidyverse)
library(timetk)
library(modeltime)
library(tidymodels)
library(workflowsets)

dataset <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales)

dataset %>% 
  group_by(id) %>%
  plot_time_series(
    .date_var    = Date, 
    .value       = Weekly_Sales, 
    .facet_ncol  = 2, 
    .interactive = F)


# Splitting ----

splits <- dataset %>% 
  time_series_split(assess = "6 months", 
                    cumulative = T)

splits %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(
    Date, Weekly_Sales, 
    .interactive = F)


# Feature Engineering ----

recipe_spec_1 <- splits %>% 
  training() %>% 
  recipe(Weekly_Sales ~ ., data = .) %>%
  step_timeseries_signature(Date) %>%
  step_rm(Date) %>%
  step_normalize(Date_index.num) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = T)


# Model Specifications ----

model_spec_xgb_1 <- boost_tree(learn_rate = 0.001) %>%
  set_engine("xgboost")

model_spec_xgb_2 <- boost_tree(learn_rate = 0.01) %>%
  set_engine("xgboost")

model_spec_xgb_3 <- boost_tree(learn_rate = 0.1) %>%
  set_engine("xgboost")

model_spec_xgb_4 <- boost_tree(learn_rate = 0.35) %>%
  set_engine("xgboost")

model_spec_xgb_5 <- boost_tree(learn_rate = 0.5) %>%
  set_engine("xgboost")

model_spec_xgb_6 <- boost_tree(learn_rate = 0.65) %>%
  set_engine("xgboost")

# A faster way
model <- tibble(
  learn_rate = c(0.001, 0.01, 0.1, 0.35, 0.5, 0.65)) %>%
  create_model_grid(f_model_spec = boost_tree,
                    engine_name = "xgboost",
                    mode = "regression")

# Extracting the model list
model_list <- model$.models

# Workflowsets 
model_wfset <- recipe_spec_1 %>% 
  list() %>% 
  workflow_set(
  models = model_list, 
  cross = T)


# Parallel Training (Fitting) ----

# Fitting Using Parallel Backend
model_parallel <- model_wfset %>%
  modeltime_fit_workflowset(data = splits %>% training(),
    control = control_fit_workflowset(verbose = T, allow_par = T))

# Accuracy Assessment
model_parallel %>%
  modeltime_calibrate(splits %>% testing()) %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = F)

# Forecast Assessment
model_parallel %>%
  modeltime_forecast(
    new_data = splits %>% testing(),
    actual_data = dataset,
    keep_data = T) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol = 2,
    .interactive = T)

# Closing Clusters
parallel_stop()
