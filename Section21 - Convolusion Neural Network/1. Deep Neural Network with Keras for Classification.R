# Deep Neural Network with Keras for Classification

# Load packages, import and prepare data ----
library(tidyverse)
library(keras)

mnist <- dataset_mnist()
x_train <- mnist$train$x #pixel values
y_train <- mnist$train$y #labels
x_test <- mnist$test$x
y_test <- mnist$test$y


# Plottin the images
x_train[5,,] %>% as.raster(max=255) %>% plot()


# reshape & rescale 
# (converting a 2D array into a 1D array for feeding into the MLP and normalising the matrix)  
x_train <- x_train %>% array_reshape(c(nrow(x_train), prod(dim(x_train)[-1]))) / 255
x_test <- x_test %>% array_reshape(c(nrow(x_test), prod(dim(x_train)[-1]))) / 255

# converting the target variable to one hot encoded vectors 
y_train <- y_train %>% to_categorical(10)
y_test <- y_test %>% to_categorical(10)

# Defining the Model ----
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 784, activation = 'relu', input_shape = 784) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')) 

# Fitting the model on the training dataset ----
history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2)

history %>% plot()

# Evaluating model on the cross validation dataset ----
model %>% evaluate(x_test, y_test)

model %>% predict_classes(x_test)

# Save the model ----
model %>% save_model_weights_hdf5("keras_model.h5")
model <- load_model_hdf5("keras_model.h5")
model %>% summary()
