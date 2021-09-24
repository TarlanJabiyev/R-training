# Import libraries & dataset ----
library(tidyverse)
library(rstudioapi)
library(inspectdf)
library(tidymodels)
library(treesnip)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

df <- read_csv('Churn_Modelling.csv')

df %>% glimpse()

df %>% inspect_na()

df <- df %>% select(-RowNumber, -CustomerId, -Surname)

df$Exited <- df$Exited %>% as_factor()

df$Exited %>% table() %>% prop.table() %>% round(2)


# Modeling ----

# Splitting the df into the Train set and Test set.
set.seed(123)
df_split <- df %>% initial_split(strata = Exited)
train <- df_split %>% training()
test <- df_split %>% testing()

# Model specifications.
model_spec <- boost_tree(
  trees = 500, 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),                
  sample_size = tune(), 
  mtry = tune(),    
  learn_rate = tune(),              
) %>% 
  set_mode("classification")

xgboost_spec <- model_spec %>% 
  set_engine("xgboost")

lightgbm_spec <- model_spec %>% 
  set_engine("lightgbm")

catboost_spec <- model_spec %>% 
  set_engine("catboost")

# Hyperparameters.
model_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  learn_rate(),
  size = 30
)

# Put the model specification into a workflow for convenience.
xgboost_wf <- workflow() %>%
  add_formula(Exited ~ .) %>%
  add_model(xgboost_spec)

lightgbm_wf <- workflow() %>% 
  add_formula(Exited ~ .) %>%
  add_model(lightgbm_spec)

catboost_wf <- workflow() %>% 
  add_formula(Exited ~ .) %>%
  add_model(catboost_spec)

# Create cross-validation resamples for tuning our model.
set.seed(123)
df_folds <- train %>% vfold_cv(v = 5, strata = Exited)

# Our tuneable workflow / our resamples.
set.seed(123)

tictoc::tic("code execution time")
xgboost_res <- tune_grid(
  xgboost_wf,
  resamples = df_folds,
  grid = model_grid,
  control = control_grid(save_pred = TRUE)
)
tictoc::toc()

tictoc::tic("code execution time")
lightgbm_res <- tune_grid(
  lightgbm_wf,
  resamples = df_folds,
  grid = model_grid,
  control = control_grid(save_pred = TRUE)
)
tictoc::toc()

tictoc::tic("code execution time")
catboost_res <- tune_grid(
  catboost_wf,
  resamples = df_folds,
  grid = model_grid,
  control = control_grid(save_pred = TRUE)
)
tictoc::toc()


# Evaluate models ----
xgboost_res %>% collect_metrics()
xgboost_res %>% show_best("roc_auc", n = 1) %>% pull(mean) %>% round(2)

lightgbm_res %>% collect_metrics()
lightgbm_res %>% show_best("roc_auc", n = 1) %>% pull(mean) %>% round(2)

catboost_res %>% collect_metrics()
catboost_res %>% show_best("roc_auc", n = 1) %>% pull(mean) %>% round(2)

# Finalize our tuneable workflow with these parameter values
final_model <- lightgbm_res %>% 
  select_best("roc_auc") %>% 
  finalize_workflow(lightgbm_wf,.)

# # Variable Importance (this only works for xgboost)
# final_model %>%
#   fit(data = train) %>%
#   pull_workflow_fit() %>%
#   vip::vip(geom = "point")


# Fit one more time to the training data and evaluate on the testing data ----
final_res <- final_model %>% last_fit(df_split)

# Confusion Matrix
final_res %>%
  collect_predictions() %>%
  conf_mat(Exited, .pred_class)

# ROC curve
auc <- final_res$.metrics %>% 
  as.data.frame() %>% .[2,3] %>% round(2)

roc_curve <- final_res %>%
  collect_predictions() %>%
  roc_curve(Exited, .pred_1) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "green") +
  geom_abline(
    lty = 3, color = "red") +
  labs(title = glue::glue('AUC = {enexpr(auc)}')) +
  hrbrthemes::theme_modern_rc()

roc_curve %>% plotly::ggplotly()
