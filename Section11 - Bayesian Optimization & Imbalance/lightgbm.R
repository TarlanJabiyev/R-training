# Import libraries & dataset ----
library(tidyverse)
library(rstudioapi)
library(inspectdf)
library(caTools)
library(Matrix)
library(lightgbm)
library(ROCR)
library(rBayesianOptimization)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

df <- read_csv('Churn_Modelling.csv')

df %>% glimpse()

df %>% inspect_na()

df <- df %>% select(-RowNumber, -CustomerId, -Surname)

df$Exited %>% table() %>% prop.table() %>% round(2)


# Splitting the df into the Train set and Test set ----
split <- df$Exited %>% sample.split(SplitRatio = 0.8)
train <- df %>% subset(split == T)
test <- df %>% subset(split == F)


# Create LGB dataset ----
features <- df %>% colnames() %>% setdiff("Exited")

categoricals <- df %>% 
  select(-Exited) %>% 
  mutate_if(is.factor, as.character) %>% 
  select_if(is.character) %>% 
  names()

train_sparse <- train %>% 
  select(features) %>% 
  as.matrix() %>% 
  Matrix(sparse = T)
test_sparse <- test %>% 
  select(features) %>% 
  as.matrix() %>% 
  Matrix(sparse = T)

lgb.train <- train_sparse %>% lgb.Dataset(label = train$Exited)


# Fitting LightGBM model ----

# Setting up LGBM parameters 
lgb.grid <- list(objective = "binary",
                 metric = "auc",
                 is_unbalance = TRUE)

# Train model
lgb.model <- lgb.train(params = lgb.grid, 
                       data = lgb.train, 
                       categorical_feature = categoricals)


# Feature importance ----
lgb.model %>% 
  lgb.importance() %>% 
  lgb.plot.importance(top_n = 5)


# Predicting the Test set results ----
y_pred <- lgb.model %>% predict(test_sparse)


# Model evaluation metrices ----

actual <- test$Exited

Threshold <- function(x,y) {
  pred = y %>% prediction(x)
  eval = pred %>% performance("acc")
  max = which.max(slot(eval, "y.values")[[1]])
  threshold = slot(eval, "x.values")[[1]][max]
  return(threshold)
}

Accuracy <- function(x,y) {
  pred = y %>% prediction(x)
  eval = pred %>% performance("acc")
  max = which.max(slot(eval, "y.values")[[1]])
  accuracy = slot(eval, "y.values")[[1]][max]
  return(accuracy)
}

AUC <- function(x,y) {
  pred = y %>% prediction(x)
  auc = pred %>% 
    performance("auc") %>% 
    slot("y.values") %>% 
    unlist() %>% round(2)
  return(auc)
}

Threshold(actual,y_pred)
Accuracy(actual,y_pred)
AUC(actual,y_pred)


# Bayesian optimization ----
lgbm_fit <- function(min_sum_hessian_in_leaf,
                     min_data_in_leaf,
                     feature_fraction,
                     bagging_fraction,
                     bagging_freq,
                     min_data,
                     lambda_l1,
                     lambda_l2,
                     min_gain_to_split,
                     nrounds,
                     max_depth,
                     num_leaves,
                     learning_rate,
                     eval_freq,
                     categorical_feature) {
  lgb.grid = list(objective = "binary",
                  metric = "auc",
                  min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
                  min_data_in_leaf = min_data_in_leaf,
                  feature_fraction = feature_fraction,
                  bagging_fraction = bagging_fraction,
                  bagging_freq = bagging_freq,
                  min_data = min_data,
                  lambda_l1 = lambda_l1,
                  lambda_l2 = lambda_l2,
                  min_gain_to_split = min_gain_to_split,
                  is_unbalance = TRUE)
  
  lgb.model = lgb.train(params = lgb.grid, 
                        data = lgb.train, 
                        nrounds = nrounds,
                        max_depth = max_depth,
                        num_leaves = num_leaves,
                        learning_rate = learning_rate,
                        eval_freq = eval_freq,
                        categorical_feature = categoricals)
  
  y_pred = lgb.model %>% predict(test_sparse)
  
  actual = test$Exited
  
  AUC = AUC(actual,y_pred)
  
  score <- list(Score = AUC,
                Pred = 0)
}

search_bound_lgbm <- list(min_sum_hessian_in_leaf = c(1L,3L),
                          min_data_in_leaf = c(20L,50L),
                          feature_fraction = c(0.1,1),
                          bagging_fraction = c(0.1,1),
                          bagging_freq = c(1L,20L),
                          min_data = c(30L,200L),
                          lambda_l1 = c(1,10),
                          lambda_l2 = c(1,10),
                          min_gain_to_split = c(5L,30L),
                          nrounds = c(100L,1500L),
                          max_depth = c(3L,10L),
                          num_leaves = c(20L,100L),
                          learning_rate = c(0.01,1),
                          eval_freq = c(10,50))

search_grid_lgbm <- data.frame(
  min_sum_hessian_in_leaf = runif(15,1L,3L),
  min_data_in_leaf = runif(15,20L,50L) %>% round(),
  feature_fraction = runif(15,0.1,1),
  bagging_fraction = runif(15,0.1,1),
  bagging_freq = runif(15,1L,20L) %>% round(),
  min_data = runif(15,30L,200L),
  lambda_l1 = runif(15,1,10),
  lambda_l2 = runif(15,1,10),
  min_gain_to_split = runif(15,5L,30L),
  nrounds = runif(15,100L,1500L),
  max_depth = runif(15,3L,10L) %>% round(),
  num_leaves = runif(15,20L,100L) %>% round(),
  learning_rate = runif(15,0.01,1),
  eval_freq = runif(15,10,50))

bayes_lgbm <- BayesianOptimization(
  FUN = lgbm_fit, 
  bounds = search_bound_lgbm,
  init_grid_dt = search_grid_lgbm,
  init_points = 5,
  n_iter = 5)

obj <- bayes_lgbm$Best_Par


# Modeling with hyperparametr tuning ----
lgb.grid <- list(objective = "binary",
                metric = "auc",
                min_sum_hessian_in_leaf = obj[1],
                min_data_in_leaf = obj[2],
                feature_fraction = obj[3],
                bagging_fraction = obj[4],
                bagging_freq = obj[5],
                min_data = obj[6],
                lambda_l1 = obj[7],
                lambda_l2 = obj[8],
                min_gain_to_split = obj[9],
                is_unbalance = TRUE)

lgb.model <- lgb.train(params = lgb.grid, 
                      data = lgb.train,
                      nrounds = obj[10],
                      max_depth = obj[11],
                      num_leaves = obj[12],
                      learning_rate = obj[13],
                      eval_freq = obj[14],
                      categorical_feature = categoricals)

# Predicting the Test set results
y_pred <- lgb.model %>% predict(test_sparse)

# Model evaluation metrices
actual <- test$Exited

Threshold(actual,y_pred)
Accuracy(actual,y_pred)
AUC(actual,y_pred)

# Feature importance
lgb.model %>% 
  lgb.importance() %>% 
  lgb.plot.importance(top_n = 5)
