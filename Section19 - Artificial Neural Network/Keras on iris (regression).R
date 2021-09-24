# Keras on iris (regression)

# Load packages, import and prepare data ----
library(tidyverse)
library(caret)
library(keras)


# Splitting ----
x_train <- iris[1:125,"Sepal.Length"]
y_train <- iris[1:125,"Petal.Width"] 
x_test <- iris[126:150,"Sepal.Length"]
y_test <- iris[126:150,"Petal.Width"]

# Defining the Model ----
model <- keras_model_sequential()
model %>%
  layer_dense(units = 8, input_shape = 1) %>% #hidden layer with 8 neurons, 1-dimensional input
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1) # output layer with linear activation

model %>% compile(
  optimizer = optimizer_adagrad(), 
  loss = "mean_squared_error")

# Fitting & evaluating ----
hist <- model %>% fit(
  x_train,
  y_train,
  batch_size = 10,
  epochs = 100)

hist %>% plot()

model %>% evaluate(x_test, y_test) #RMSE

preds <- model %>% predict(x_test)
preds <- preds %>% as.data.frame() %>% pull(V1)
preds
