# Import libraries & dataset ----
library(tidyverse)
library(rstudioapi)
library(inspectdf)
library(caTools)
library(xgboost)
library(parsnip)
library(rBayesianOptimization)
library(Metrics)
set.seed(123)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

df <- read_csv('50_Startups.csv')

df %>% glimpse()

df %>% inspect_na()


# Splitting the df into the Train set and Test set ----
split <- df$Profit %>% sample.split(SplitRatio = 0.8)
train <- df %>% subset(split == T)
test <- df %>% subset(split == F)


# Fitting XGBoost model ----
regression <- boost_tree(
  mode = "regression") %>% 
  set_engine(engine = "xgboost") %>%
  fit(Profit ~ ., data = train)


# Predicting the Test set results ----
pred <- regression %>% 
  predict(test %>% select(-Profit)) %>% 
  pull(.pred)


# Model evaluation metrices ----
actual <- test$Profit

eval_func <- function(x, y) summary(lm(y~x))
eval_sum <- eval_func(actual,pred)

eval_sum$adj.r.squared %>% round(2)
mae(actual,pred) %>% round(1)
rmse(actual,pred) %>% round(1)


# Bayesian optimization ----
xgboost_fit <- function(mtry,
                        trees,
                        learn_rate,  
                        tree_depth) {
  regression = boost_tree(
    mode = "regression", 
    mtry = mtry,
    trees = trees,
    learn_rate = learn_rate,  
    tree_depth = tree_depth) %>% 
    set_engine(engine = "xgboost") %>%
    fit(Profit ~ ., data = train)
  
  pred = regression %>% 
    predict(test %>% select(-Profit)) %>% 
    pull(.pred)
  
  actual = test$Profit
  
  eval_sum = eval_func(actual,pred)
  
  score <- list(Score = eval_sum$adj.r.squared,
                Pred = 0)
}

search_bound_xgboost <- list(mtry = c(10L,100L),
                             trees = c(10L,150L),
                             learn_rate = c(0.01,0.5),  
                             tree_depth = c(2L,10L))

search_grid_xgboost <- data.frame(
  mtry = runif(30,10L,100L),
  trees = runif(30,10L,150L),
  learn_rate = runif(30,0.01,0.5),  
  tree_depth = runif(30,2L,10L) %>% round())

bayes_xgboost <- BayesianOptimization(
  FUN = xgboost_fit, 
  bounds = search_bound_xgboost,
  init_grid_dt = search_grid_xgboost,
  init_points = 5, 
  n_iter = 5)

obj <- bayes_xgboost$Best_Par


# Modeling with hyperparametr tuning ----
regression <- boost_tree(
  mode = "regression", 
  mtry = obj[1],
  trees = obj[2],
  learn_rate = obj[3],  
  tree_depth = obj[4]) %>% 
  set_engine(engine = "xgboost") %>%
  fit(Profit ~ ., data = train)

# Predicting the Test set results
pred <- regression %>% 
  predict(test %>% select(-Profit)) %>% 
  pull(.pred)

# Model evaluation metrices
actual <- test$Profit

eval_sum <- eval_func(actual,pred)

eval_sum$adj.r.squared %>% round(2)
mae(actual,pred) %>% round(1)
rmse(actual,pred) %>% round(1)
