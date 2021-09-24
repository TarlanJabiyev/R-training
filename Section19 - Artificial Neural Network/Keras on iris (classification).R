# Keras on iris (classification)

# Load packages, import and prepare data ----
library(tidyverse)
library(caret)
library(keras)

index <- iris$Species %>% createDataPartition(p=0.7,list=F)

train_x <- data.matrix(iris[index,-5]) 
train_y <- iris[index,5] 

test_x <- data.matrix(iris[-index,-5]) 
test_y <- iris[-index,5]

# converting the target variable to one hot encoded vectors 
train_y <- train_y %>% as.numeric() %>% to_categorical() %>% .[,-1]
test_y <- test_y %>% as.numeric() %>% to_categorical() %>% .[,-1]

# Normalize the predictor values: predictor values should be between 0-1
train_x <- apply(train_x, 2, function(x) (x-min(x))/(max(x) - min(x))) %>% as.matrix()
test_x <- apply(test_x, 2, function(x) (x-min(x))/(max(x) - min(x))) %>% as.matrix()

# Defining the Model ----
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
  layer_dense(units = 3, activation = 'softmax') #output

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adagrad(),
  metrics = c('accuracy'))

# Fitting & evaluating ----
history <- model %>% fit(
  train_x, train_y,
  epochs = 100, 
  batch_size = 5,
  validation_split = 0.1,
  shuffle = T)

model %>% evaluate(test_x,test_y)
