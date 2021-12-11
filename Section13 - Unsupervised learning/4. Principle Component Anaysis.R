# Import libraries & dataset ----
library(tidyverse)
library(rstudioapi)
library(h2o)
library(glue)
library(highcharter)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

df <- read_csv('Drinks.csv')


# EDA ----
df %>% glimpse()

df$Customer_Segment <- df$Customer_Segment %>% as_factor()

df$Customer_Segment %>% table() %>% prop.table()


# PCA with h2o ----
h2o.init()

df.h2o <- df %>% as.h2o()

model_pca <- df.h2o %>% 
  h2o.prcomp(transform = "STANDARDIZE",
             k = 4, seed = 123,
             impute_missing = T,
             max_runtime_secs = 90)

model_pca %>% 
  h2o.predict(df.h2o) %>% 
  as_tibble() %>% 
  add_column(Customer_Segment=df$Customer_Segment) -> pca_pred


model_pca@model$model_summary %>% as.data.frame() -> table
table

table[2,] %>% 
  gather() %>% 
  hchart("bar", hcaes(x = key, y = value)) %>%
  hc_colors(colors = 'blue') %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)

pca_pred %>% 
  ggplot(aes(x = PC1, y = PC2, color = Customer_Segment)) +
  geom_point() + 
  labs(x=glue('PC1 ~ {table$pc1[2] %>% round(2)}'),
       y=glue('PC2 ~ {table$pc2[2] %>% round(2)}')) +
  coord_equal() +
  ggthemes::theme_few(base_family = "Laksaman") +
  facet_wrap(~ Customer_Segment, nrow = 2) +
  gghighlight::gghighlight()

pca_pred %>% select(-Customer_Segment) %>% 
  cbind(df %>% select(-Customer_Segment)) %>% 
  scale(center=T, scale=T) %>% 
  as_tibble() %>% 
  cor() %>% 
  .[-c(1:4),1:4] %>% 
  round(2) %>% 
  hchart(label=T)


# Model after PCA ----

df_pca <- pca_pred %>% as.h2o()

y <- "Customer_Segment"
x <- pca_pred %>% select(-Customer_Segment) %>% names()

model <- h2o.automl(
  x=x, y=y,
  training_frame=df_pca,
  stopping_metric = "mean_per_class_error",
  seed=123,
  max_runtime_secs=90)

model@leader %>% h2o.confusionMatrix()

model@leaderboard %>% as_tibble()
