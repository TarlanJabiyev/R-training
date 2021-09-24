# Import libraries & dataset ----
library(tidyverse)
library(rstudioapi)
library(inspectdf)
library(tidymodels)
library(kknn)
library(plotly)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

df <- read_csv('Churn_Modelling.csv')

df %>% glimpse()

df %>% inspect_na()

df <- df %>% select(-RowNumber, -CustomerId, -Surname)

df$Exited <- df$Exited %>% as.factor()

df$Exited %>% table() %>% prop.table() %>% round(2)


# Modeling ----

# Splitting data into training and testing sets
set.seed(123)
df_split <- df %>% initial_split(strata = Exited)
train <- df_split %>% training()
test <- df_split %>% testing()


# Create bootstrap resamples of the training data to evaluate our models
set.seed(123)
df_boot <- train %>% bootstraps()

# Let’s compare two different models. We start by creating the model specifications.
glm_spec <- logistic_reg() %>% 
  set_engine("glm") # glmnet, keras
glm_spec

svm_spec <- svm_rbf() %>% 
  set_mode("classification") %>% 
  set_engine("kernlab") %>% #liquidSVM
  translate()

knn_spec <- nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification") %>% 
  translate()

dt_spec <- decision_tree() %>% 
  set_mode("classification") %>% 
  set_engine("rpart") %>% # C5.0, spark
  translate()

rf_spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("randomForest") #ranger, spark


# Put the model specification into a workflow for convenience. 
# We can add a model, and the fit to each of the resamples.
df_wf <- workflow() %>%
  add_formula(Exited ~ .)

# First, we can fit the logistic regression model.
tictoc::tic("code execution time")
glm_rs <- df_wf %>%
  add_model(glm_spec) %>%
  fit_resamples(
    resamples = df_boot,
    control = control_resamples(save_pred = TRUE)
  )
tictoc::toc()

# Second, we can fit the support vector machines model.
tictoc::tic("code execution time")
svm_rs <- df_wf %>%
  add_model(svm_spec) %>%
  fit_resamples(
    resamples = df_boot,
    control = control_resamples(save_pred = TRUE)
  )
tictoc::toc()

# Third, we can fit the k-nearest neighbor model.
tictoc::tic("code execution time")
knn_rs <- df_wf %>%
  add_model(knn_spec) %>%
  fit_resamples(
    resamples = df_boot,
    control = control_resamples(save_pred = TRUE)
  )
tictoc::toc()

# Fourth, we can fit the decision tree model.
tictoc::tic("code execution time")
dt_rs <- df_wf %>%
  add_model(dt_spec) %>%
  fit_resamples(
    resamples = df_boot,
    control = control_resamples(save_pred = TRUE)
  )
tictoc::toc()

# Fifth, we can fit the random forest model.
tictoc::tic("code execution time")
rf_rs <- df_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = df_boot,
    control = control_resamples(save_pred = TRUE)
  )
tictoc::toc()

# We have fit each of our candidate models to our resampled training set!


# Evaluate model ----

glm_rs %>% collect_metrics()

svm_rs %>% collect_metrics()

knn_rs %>% collect_metrics()

dt_rs %>% collect_metrics()

rf_rs %>% collect_metrics()


# Fit one more time to the training data and evaluate on the testing data ----

final_res <- df_wf %>%
  add_model(glm_spec) %>%
  last_fit(df_split)

final_res %>% collect_metrics()

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
