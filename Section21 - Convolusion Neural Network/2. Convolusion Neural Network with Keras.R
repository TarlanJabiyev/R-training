# Convolusion Neural Network with Keras

# Load packages, import and prepare data ----
library(tidyverse)
library(keras)

fashion_mnist <- dataset_fashion_mnist()

# Train & Test sets
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test


# Plottin the images
train_images[5,,] %>% as.raster(max=255) %>% plot()


class_names <- c('T-Shirt','Trouser','Pullover','Dress','Coat',
                 'Sandal','Shirt','Sneaker','Bag','Ankle boot')

class_names[train_labels[5]+1] #because index started with 0


# The data are on the scale 0-255. Normalize to 0-1.
train_images <- train_images / 255
test_images <- test_images / 255


# Creating validation split from train split 
valid_images <-  train_images[1:10000,,]
part_train_images <- train_images[10001:60000,,]
valid_labels <- train_labels[1:10000]
part_train_labels <- train_labels[10001:60000]

part_train_images %>% glimpse()

part_train_images <- part_train_images %>% array_reshape(c(50000,28,28,1))
valid_images <- valid_images %>% array_reshape(c(10000,28,28,1))
test_images <- test_images %>% array_reshape(c(10000,28,28,1))


# Defining the Model ----
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32,
                kernel_size = c(3, 3),
                activation = 'relu',
                input_shape = c(28, 28, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2))

model <- model %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dense(units = 10, activation = 'softmax')


# Configuring the Model ----
model %>% compile(
  optimizer = 'sgd',#sgd - stochastic gradient descent
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy'))
#'sparse_categorical_crossentropy': more than 2 classes and observation can belong to only one class
#'Binary_crossentropy': 2 classes and object belongs to one of the two classes
#'Categorical_crossentropy': more than 2 classes and observation can belong to multiple classes


# Fitting the model on the training dataset ----
model %>% fit(
  part_train_images,
  part_train_labels,
  epochs = 20, 
  batch_size = 50,
  validation_data = list(valid_images,valid_labels))


# Evaluating model ----
cnn_score <- model %>% evaluate(test_images,test_labels)

cnn_score$loss #test loss
cnn_score$acc #test accuracy


# Predicting ----
pred <- model %>% predict_classes(test_images)
pred[1:20]

class_names[pred[1:20]+1]
class_names[test_labels[1:20]+1]
