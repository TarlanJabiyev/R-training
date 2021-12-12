# Installation ----

# devtools::install_github("rstudio/sparklyr")
# library(sparklyr)
# 
# options(timeout = 1e5); getOption('timeout')
# spark_install()
# 
# install.packages("rsparkling", type = "source",
#                  repos = "http://h2o-release.s3.amazonaws.com/sparkling-water/spark-2.1/3.32.1.7-1-2.1/R")


# ML Pipelines ----

library(tidyverse)
library(sparklyr)

sc <- spark_connect(master = "local")
iris_tbl <- sc %>% 
  copy_to(iris, "iris")

# split the data into train and validation sets
iris_data <- iris_tbl %>%
  sdf_random_split(train = 2/3, validation = 1/3, seed = 123)

pipeline <- sc %>% 
  ml_pipeline() %>%
  ft_dplyr_transformer(iris_data$train %>%
                         mutate(Sepal_Length = log(Sepal_Length),
                                Sepal_Width = Sepal_Width ^ 2)) %>%
  ft_string_indexer("Species", "label")

pipeline_model <- pipeline %>%
  ml_fit(iris_data$train)

# pipeline_model is a transformer
pipeline_model %>%
  ml_transform(iris_data$validation)


# A predictive modeling pipeline ----

# define stages
# vector_assember will concatenate the predictor columns into one vector column
vector_assembler <- sc %>% 
  ft_vector_assembler(input_cols = setdiff(colnames(iris_data$train), "Species"), 
                      output_col = "features")

logistic_regression <- sc %>% 
  ml_logistic_regression()

# obtain the labels from the fitted StringIndexerModel
labels <- pipeline_model %>%
  ml_stage("string_indexer") %>%
  ml_labels()

# IndexToString will convert the predicted numeric values back to class labels
index_to_string <- sc %>% 
  ft_index_to_string("prediction", "predicted_label", 
                     labels = labels)

# construct a pipeline with these stages
prediction_pipeline <- pipeline %>% 
  ml_pipeline(vector_assembler, 
              logistic_regression,
              index_to_string)

# fit to data and make some predictions
prediction_model <- prediction_pipeline %>%
  ml_fit(iris_data$train)

pred_table <- prediction_model %>%
  ml_transform(iris_data$validation)

pred_table <- pred_table %>%
  select(Species, label:predicted_label)

# model persistence
prediction_model %>% 
  ml_save("/Users/tarlanjabiyev/Desktop/prediction_model")


# Bisecting K-means ----

model <- iris_tbl %>% 
  ml_bisecting_kmeans(Species ~ Petal_Length + Petal_Width, k = 3, seed = 123)

predictions <- model %>% 
  ml_predict(iris_tbl) %>%
  collect() %>%
  mutate(cluster = as.factor(prediction))

predictions %>% 
  ggplot(aes(Petal_Length, Petal_Width, 
  color = predictions$cluster)) + 
  geom_point()


# Frequent pattern mining ----

# create an item purchase history dataset
items <- data.frame(items = c("1,2,5", "1,2,3,5", "1,2"),
                    stringsAsFactors = F)

# parse into vector column
items_tbl <- sc %>% 
  copy_to(items) %>%
  mutate(items = split(items, ","))

# fit the model
fp_model <- items_tbl %>%
  ml_fpgrowth(min_support = 0.5, min_confidence = 0.6)

# use the model to predict related items based on learned association rules
fp_model %>%
  ml_transform(items_tbl) %>%
  collect() %>%
  mutate_all(function(x) sapply(x, paste0, collapse = ","))

