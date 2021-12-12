# Installation ----

# options(timeout = 1e5); getOption('timeout')
# 
# # https://docs.h2o.ai/sparkling-water/3.1/latest-stable/doc/rsparkling.html
# 
# # Install Spark via Sparklyr
# devtools::install_github("rstudio/sparklyr")
# library(sparklyr)
# spark_install(version = "3.1.2")
# 
# # Install H2O
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# pkgs <- c("methods", "statmod", "stats", "graphics", "RCurl", "jsonlite", "tools", "utils")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# install.packages("h2o", type = "source", repos = "http://h2o-release.s3.amazonaws.com/h2o/rel-zizler/4/R")
# 
# # Install RSparkling
# install.packages("rsparkling", type = "source", 
#                  repos = "http://h2o-release.s3.amazonaws.com/sparkling-water/spark-3.1/3.34.0.4-1-3.1/R")


# Using H2O ----

library(tidyverse)
library(sparklyr)
library(rsparkling)
library(h2o)

sc <- spark_connect(master = "local", version = "3.1.2")

h2oConf <- H2OConf()

h2oConf$set("spark.ext.h2o.cloud.name", "mycloud")

hc <- h2oConf %>% 
  H2OContext.getOrCreate()

hc$openFlow()

mtcars_tbl <- sc %>% 
  copy_to(mtcars, "mtcars")

mtcars_hf <- mtcars_tbl %>% 
  hc$asH2OFrame()

# Data Preparations
y <- "mpg"
x <- mtcars_hf %>% names() %>% setdiff(y)

splits <- mtcars_hf %>% 
  h2o.splitFrame(ratios = 0.7, seed = 123)

# Model Training
fit <- h2o.gbm(x = x,
               y = y,
               training_frame = splits[[1]],
               min_rows = 1,
               seed = 123)

# Model Performance
perf <- fit %>% 
  h2o.performance(splits[[2]])

# Predictions
pred_hf <- fit %>% 
  h2o.predict(splits[[2]])

# Convert an H2OFrame into a Spark DataFrame
pred_sdf <- hc$asSparkFrame(pred_hf)


# GENERALIZED LINEAR MODEL ----

# fit a linear model to the training dataset
glm_model <- h2o.glm(x = c("wt", "cyl"), 
                     y = "mpg", 
                     training_frame = splits[[1]],
                     lambda_search = T)

# compute predicted values on our test dataset
pred <- glm_model %>% 
  h2o.predict(splits[[2]])

# convert from H2O Frame to Spark DataFrame
predicted <- hc$asSparkFrame(pred)

# extract the true 'mpg' values from our test dataset
actual <- splits[[2]] %>%
  as.data.frame() %>% 
  pull(mpg)

# produce a data.frame housing our predicted + actual 'mpg' values
data <- data.frame(predicted = predicted, actual = actual)

# a bug in data.frame does not set colnames properly; reset here 
names(data) <- c("predicted", "actual")

# plot predicted vs. actual values
data %>% 
  ggplot(aes(x = actual, y = predicted)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  labs(x = "Actual Fuel Consumption",
       y = "Predicted Fuel Consumption",
       title = "Predicted and Actual Fuel Consumption")


# K-MEANS CLUSTERING ----

iris_tbl <- sc %>% 
  copy_to(iris, "iris", overwrite = T)

iris_hf <- iris_tbl %>% 
  hc$asH2OFrame()

kmeans_model <- iris_hf %>% 
  h2o.kmeans(x = 3:4,
             k = 3,
             seed = 123)

# print the cluster centers
kmeans_model %>% 
  h2o.centers()

# print the centroid statistics
kmeans_model %>% 
  h2o.centroid_stats()


# PCA ----

pca_model <- iris_hf %>% 
  h2o.prcomp(x = 1:4,
             k = 4,
             seed = 123)


# RANDOM FOREST ----

y <- "Species"
x <- iris_hf %>% names() %>% setdiff(y)
iris_hf[,y] <- iris_hf[,y] %>% as.factor()

splits <- iris_hf %>% 
  h2o.splitFrame(seed = 123)

rf_model <- h2o.randomForest(x = x, 
                             y = y,
                             training_frame = splits[[1]],
                             validation_frame = splits[[2]],
                             nbins = 32,
                             max_depth = 5,
                             ntrees = 20,
                             seed = 123)

rf_model %>% 
  h2o.confusionMatrix(valid = T)

rf_model %>% 
  h2o.varimp_plot()


# GRADIENT BOOSTING MACHINE ----

gbm_model <- h2o.gbm(x = x, 
                     y = y,
                     training_frame = splits[[1]],
                     validation_frame = splits[[2]],                     
                     ntrees = 20,
                     max_depth = 3,
                     learn_rate = 0.01,
                     col_sample_rate = 0.7,
                     seed = 123)

gbm_model %>% 
  h2o.confusionMatrix(valid = T)


# DEEP LEARNING ----

path <- system.file("extdata", "prostate.csv", package = "h2o")
prostate_df <- sc %>% 
  spark_read_csv("prostate", path)

prostate_hf <- prostate_df %>% 
  hc$asH2OFrame()

splits <- prostate_hf %>% 
  h2o.splitFrame(seed = 123)

y <- "VOL"
x <- prostate_hf %>% names() %>% setdiff(c("ID", y))

dl_fit <- h2o.deeplearning(x = x, 
                           y = y,
                           training_frame = splits[[1]],
                           epochs = 15,
                           activation = "Rectifier",
                           hidden = c(10, 5, 10),
                           input_dropout_ratio = 0.7)

dl_fit %>% 
  h2o.performance(splits[[2]])


# CARTESIAN GRID SEARCH ----

# GBM hyperparamters
gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 5, 9),
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(0.2, 0.5, 1.0))

# Train and validate a grid of GBMs
gbm_grid1 <- h2o.grid("gbm", 
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid1",
                      training_frame = splits[[1]],
                      validation_frame = splits[[1]],
                      ntrees = 100,
                      seed = 123,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation MSE
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1", 
                             sort_by = "mse", 
                             decreasing = F)


# RANDOM GRID SEARCH ----

# GBM hyperparamters
gbm_params2 <- list(learn_rate = seq(0.01, 0.1, 0.01),
                    max_depth = seq(2, 10, 1),
                    sample_rate = seq(0.5, 1.0, 0.1),
                    col_sample_rate = seq(0.1, 1.0, 0.1))

search_criteria2 <- list(strategy = "RandomDiscrete", 
                         max_models = 50)

# Train and validate a grid of GBMs
gbm_grid2 <- h2o.grid("gbm", 
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid2",
                      training_frame = splits[[1]],
                      validation_frame = splits[[2]],
                      ntrees = 100,
                      seed = 123,
                      hyper_params = gbm_params2,
                      search_criteria = search_criteria2)

# Get the grid results, sorted by validation MSE
gbm_gridperf2 <- h2o.getGrid(grid_id = "gbm_grid2", 
                             sort_by = "mse", 
                             decreasing = F)

gbm_gridperf2@summary_table[1,]


# Exporting Models ----

my_model %>% 
  h2o.saveModel(path = "/Users/tarlanjabiyev/Desktop/h2omodels")


sc %>% spark_disconnect()
