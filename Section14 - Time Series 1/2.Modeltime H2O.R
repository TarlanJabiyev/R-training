# Import libraries & dataset ----
library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime.h2o)
library(rstudioapi)

df <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales)


# EDA ----

df %>% 
  group_by(id) %>% 
  plot_time_series(
    Date, Weekly_Sales,
    .facet_ncol  = 2,
    .smooth      = F,
    .interactive = T)


# Function to get average sale by week, month and quarter
# If we use the mean(Weekly_Sales) daily we get the actual values
# Since values are unique
convert_date_ts <- function(data, unit = "day"){
  new_data <- data %>% 
    mutate(Date = floor_date(Date, unit = unit)) %>% 
    group_by(Date, id) %>% 
    summarise(Weekly_Sales = mean(Weekly_Sales)) %>% 
    ungroup()
  return(new_data)}

# Weekly
df %>% 
  convert_date_ts(unit = "week") %>% 
  group_by(id) %>% 
  plot_time_series(
    .date_var = Date,
    .value = Weekly_Sales,
    .facet_ncol = 2,
    .smooth_size = 0.5)

# Monthly
df %>% 
  convert_date_ts(unit = "month") %>% 
  group_by(id) %>% 
  plot_time_series(
    .date_var = Date,
    .value = Weekly_Sales,
    .facet_ncol = 2,
    .smooth_size = 0.5)

# Quarterly
df %>% 
  convert_date_ts(unit = "quarter") %>% 
  group_by(id) %>% 
  plot_time_series(
    .date_var = Date,
    .value = Weekly_Sales,
    .facet_ncol = 2,
    .smooth_size = 0.5)

# Yearly
df %>% 
  convert_date_ts(unit = "year") %>% 
  group_by(id) %>% 
  plot_time_series(
    .date_var = Date,
    .value = Weekly_Sales,
    .facet_ncol = 2,
    .smooth_size = 0.5)


# Seasonal and Trend decomposition
plot_seasonal_decomposition <- function(data, interactive = F){
  stl_plot <- data %>% 
    group_by(id) %>% 
    plot_stl_diagnostics(
      .date_var = Date,
      .value = log1p(Weekly_Sales),
      .interactive = interactive)
  return(stl_plot)
}

df %>% 
  filter(id %in% c("1_3")) %>% 
  plot_seasonal_decomposition(interactive = T)


# Outlier Detection
df %>%
  group_by(id) %>%
  plot_anomaly_diagnostics(
    .date = Date,
    .value = Weekly_Sales,
    .facet_ncol = 2,
    .interactive = T,
    .title = "Anomaly Diagnostics Dow Jones",
    .anom_color ="#FB3029", 
    .max_anomalies = 0.07, 
    .alpha = 0.05)


# Seasonality Evaluation
df %>%
  filter(id == "1_8") %>% 
  plot_seasonal_diagnostics(
    Date, Weekly_Sales,
    .feature_set = c("week", "month.lbl"),
    .interactive = T)


# Splitting ----
splits <- df %>% 
  time_series_split(assess = "3 month", cumulative = T)

recipe_spec <- recipe(Weekly_Sales ~ ., training(splits)) %>%
  step_timeseries_signature(Date) 

train <- training(splits) %>% bake(prep(recipe_spec),.)
test  <- testing(splits) %>% bake(prep(recipe_spec),.)


# Modeling ----

h2o.init()

# Specification
model_spec_h2o <- automl_reg(mode = 'regression') %>%
  set_engine(
    'h2o', max_runtime_secs = 360,
    nfolds = 5, seed = 123, 
    verbosity = NULL, max_models = 3, 
    max_runtime_secs_per_model = 3) #exclude_algos = c("DeepLearning") 

# Training
model_fit_h2o <- model_spec_h2o %>%
  fit(Weekly_Sales ~ ., train)

# Prediction
Prediction <- model_fit_h2o %>% predict(test)


# Modeltime Workflow ----

modeltime <- model_fit_h2o %>% modeltime_table() 

# Calibrate to the testing set and visualize the forecasts
modeltime %>%
  modeltime_calibrate(test) %>%
  modeltime_forecast(
    new_data = test,
    actual_data = df,
    keep_data = T
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol = 2, 
    .interactive = T)


# Refit to Full Dataset & Forecast Forward ----

data_prepared <- bind_rows(train, test)

future <- data_prepared %>%
  group_by(id) %>%
  future_frame(.length_out = "1 year") %>%
  ungroup()

future_prepared <- recipe_spec %>% prep() %>% bake(future)

refit <- modeltime %>%
  modeltime_refit(data_prepared)

# Visualize the final forecast
refit %>%
  modeltime_forecast(
    new_data = future_prepared,
    actual_data = data_prepared,
    keep_data = T
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol = 2,
    .interactive = T)


# Saving and Loading Models ----
path <- dirname(getSourceEditorContext()$path)

model_fit_h2o %>% 
  save_h2o_model(path = paste0(path,"/model_fit_h2o"), overwrite = T)

model <- load_h2o_model(path = paste0(path,"/model_fit_h2o"))
