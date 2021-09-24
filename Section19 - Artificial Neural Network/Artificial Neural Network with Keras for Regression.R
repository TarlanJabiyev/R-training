# Artificial Neural Network with Keras for Regression

# Load packages, import and prepare data ----
library(tidyverse)
library(keras)

boston_housing <- dataset_boston_housing()

# Train & Test sets
c(train_data, train_labels) %<-% boston_housing$train
c(test_data, test_labels) %<-% boston_housing$test

train_df <- train_data %>% as.data.frame()
test_df <- test_data %>% as.data.frame()

column_names <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 
                  'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')
colnames(train_df) <- column_names
colnames(test_df) <- column_names

# Normalize features
train_df <- train_df %>% scale() 
test_df <- test_df %>% scale() 

# Defining the Model ----
model <- keras_model_sequential()
model %>%
  layer_dense(units = 64, activation = "relu",
              input_shape = dim(train_data)[2]) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1) #output

model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_absolute_error"))

# Fitting & evaluating ----
history <- model %>% fit(
  train_data,
  train_labels,
  batch_size = 10,
  epochs = 100,
  validation_split = 0.2)

history %>% plot()

model %>% evaluate(test_df, test_labels) #RMSE

preds <- model %>% predict(test_df)
preds <- preds %>% as.data.frame() %>% pull(V1)
preds
