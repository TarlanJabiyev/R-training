# Autoencoders for Unsupervised Classification

# Load packages, import and prepare data ----
library(tidyverse)
library(data.table)
library(h2o)

tumor <- fread("cancer_tumor.csv") 
#target is 'diagnosis': predict if the tumor is malignant (M) or benign (B)

tumor %>% glimpse()

df <- tumor %>% select(-id,-V33)

df$diagnosis %>% table() %>% prop.table()


# Modeling ----
h2o.init()
df_h2o <- df %>% as.h2o()

model <- h2o.deeplearning(
  x = 2:31,
  training_frame = df_h2o,
  hidden = c(400, 200, 2, 200, 400),
  epochs = 300,
  activation = "Tanh",
  autoencoder = T)

train_supervised_features2 <- model %>% h2o.deepfeatures(df_h2o, layer=3)

train_supervised_features2 %>% 
  as.data.frame() %>% 
  mutate(label = df$diagnosis) %>% 
  ggplot(aes(DF.L3.C1, DF.L3.C2, color = label)) + 
  geom_point() +
  labs(title = "Neural network: 400 - 200 - 2 - 200 - 4000")

