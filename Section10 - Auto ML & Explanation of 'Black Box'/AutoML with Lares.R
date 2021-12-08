# https://cran.r-project.org/web/packages/lares/vignettes/h2o_automl.html

library(tidyverse)
library(lares)

df <- dft %>% 
  select(-Ticket, -PassengerId, -Cabin)


# Classification: Binary ----

model <- df %>% 
  h2o_automl(y = Survived, 
             max_models = 1, 
             impute = F, 
             target = "TRUE")

model %>% plot()

model$metrics

model$plots$metrics$gains
model$plots$metrics$response
model$plots$metrics$conf_matrix
model$plots$metrics$ROC

model$importance
model$plots$importance


# Classification: Multi-Categorical ----

model <- df %>% 
  h2o_automl(Pclass, 
             ignore = c("Fare", "Cabin"),
             max_time = 30, 
             plots = F)

model %>% plot()


# Regression ----

model <- df %>% 
  h2o_automl(y = "Fare", 
             ignore = "Pclass", 
             exclude_algos = NULL, 
             quiet = T)

model %>% print()

model %>% plot()
