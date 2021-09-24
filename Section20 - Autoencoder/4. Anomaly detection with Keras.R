# Anomaly detection with Keras

# Load packages, import and prepare data ----
library(tidyverse)
library(data.table)
library(caret)
library(keras)

tumor <- fread("cancer_tumor.csv") 
#target is 'diagnosis': predict if the tumor is malignant (M) or benign (B)

tumor %>% glimpse()

df <- tumor %>% select(-id,-V33)

df$diagnosis %>% table() %>% prop.table()

# Splitting ----
index <- df$diagnosis %>% createDataPartition(p=0.7,list=F)

train_x <- df[index,-1] %>% data.matrix()  
train_y <- df[index,1] 

test_x <- df[-index,-1] %>% data.matrix() 
test_y <- df[-index,1]

# rescale: (0-1)
train_x <- apply(train_x, 2, function(x) (x-min(x))/(max(x) - min(x))) %>% as.matrix()
test_x <- apply(test_x, 2, function(x) (x-min(x))/(max(x) - min(x))) %>% as.matrix()


# Create the autoencoder ----
input_dim = 30 ; inner_layer_dim = 10

input_layer <- layer_input(shape = c(input_dim)) #predictors
encoder <- layer_dense(units = inner_layer_dim, activation='relu')(input_layer)
decoder <- layer_dense(units = 30)(encoder)

autoencoder <- keras_model(inputs = input_layer, outputs = decoder)

autoencoder %>% compile(
  optimizer = optimizer_adagrad(), 
  loss = 'mean_squared_error',
  metrics = c('accuracy'))


# Fitting ----
history <- autoencoder %>% fit(
  train_x, train_x, 
  epochs = 50, 
  batch_size = 256, 
  validation_split = 0.2)

history %>% plot()


# Evaluating ----
preds <- autoencoder %>% predict(test_x)

error <- (preds-test_x)^2 %>% rowSums()
eval <- data.frame(error = error, class = test_y$diagnosis)

eval %>% 
  group_by(class) %>% 
  summarise(avg_error = mean(error)) %>% 
  ggplot(aes(x=class, fill=class, y=avg_error)) +
  geom_boxplot()


threshold <- 2.1 # deciding what is an error or not
y_preds <- ifelse(error>threshold,"M","B")

cm <- table(y_preds,test_y$diagnosis) # confusion matrix

cm %>% 
  as.data.frame() %>% 
  filter(y_preds == Var2) %>% 
  pull(Freq) %>% 
  sum() -> true
  
cm %>% 
  as.data.frame() %>% 
  pull(Freq) %>% 
  sum() -> all

true/all
