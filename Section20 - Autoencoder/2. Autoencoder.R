# Autoencoder

# Load packages, import and prepare data ----
library(tidyverse)
library(h2o)
h2o.init()

# Import ECG train and test data into the H2O cluster
train_ecg <- h2o.importFile(
  path = "http://h2o-public-test-data.s3.amazonaws.com/smalldata/anomaly/ecg_discord_train.csv", 
  header = F, sep = ",")
test_ecg <- h2o.importFile(
  path = "http://h2o-public-test-data.s3.amazonaws.com/smalldata/anomaly/ecg_discord_test.csv", 
  header = F, sep = ",")


# Modeling ----
anomaly_model <- h2o.deeplearning(
  x = names(train_ecg),
  training_frame = train_ecg,
  activation = "Tanh",
  autoencoder = T,
  hidden = c(50,20,50),
  sparse = T,
  l1 = 1e-4,
  epochs = 1)

anomaly_model %>% h2o.varimp_plot(num_of_features = 10)

recon_error <- anomaly_model %>% h2o.anomaly(test_ecg) %>% as.data.frame()

recon_error %>% plot.ts()

recon_error %>% 
  ggplot(aes(x=rownames(recon_error),y=sort(Reconstruction.MSE)))+
  geom_point() + ylab("Reconstruction.MSE")

test_ecg[which(recon_error > 3),] 
