# Import libraries & dataset ----
library(tidyverse) 
library(data.table)
library(rstudioapi)
library(inspectdf)
library(h2o) 
library(Metrics)
library(glue)
library(plotly)
library(patchwork)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

raw <- fread("Life Expantancy Data.csv")

raw %>% glimpse()

raw %>% inspect_na()

names(raw) <- names(raw) %>% 
  str_replace_all(" ","_") %>% 
  str_replace_all("-","_") %>% 
  str_replace_all("/","_")


# --------------------------------- Modeling ----------------------------------
h2o.init()

h2o_data <- raw %>% as.h2o()


# Splitting the data ----
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'Life_expectancy'
features <- raw %>% select(-Life_expectancy) %>% names()


# Fitting h2o model ----
model <- h2o.automl(
  x = features,
  y = target,
  training_frame    = train,
  validation_frame  = test,
  leaderboard_frame = test,
  stopping_metric = "RMSE",
  seed = 123,
  max_runtime_secs = 360)

model@leaderboard %>% as.data.frame()
model <- model@leader


# Predicting the Test set results ----
y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
pred <- y_pred$predict


# ----------------------------- Model evaluation -----------------------------
test_set <- test %>% as.data.frame()
actual <- test_set$Life_expectancy

eval_func <- function(x, y) summary(lm(y~x))
eval_sum <- eval_func(actual,pred)

eval_sum$adj.r.squared %>% round(2)
mae(actual,pred) %>% round(1)
rmse(actual,pred) %>% round(1)


# Plotting actual & predicted ----
my_data <- cbind(pred,actual) %>% 
  as.data.frame()

Adjusted_R2 <- eval_sum$adj.r.squared

g <- my_data %>% 
  ggplot(aes(pred, actual)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Test: Adjusted R2 = {round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g %>% ggplotly()


# Check overfitting ----
y_pred_train <- model %>% h2o.predict(newdata = train) %>% as.data.frame()
pred_train <- y_pred_train$predict

train_set <- train %>% as.data.frame()
actual_train <- train_set$Life_expectancy

eval_sum <- eval_func(actual_train,pred_train)

eval_sum$adj.r.squared %>% round(2)
mae(actual_train,pred_train) %>% round(1)
rmse(actual_train,pred_train) %>% round(1)


# Plotting actual & predicted
my_data_train <- cbind(pred_train,actual_train) %>% 
  as.data.frame()

Adjusted_R2_train <- eval_sum$adj.r.squared

g_train <- my_data_train %>% 
  ggplot(aes(pred_train, actual_train)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Train: Adjusted R2 = {round(enexpr(Adjusted_R2_train),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g_train %>% ggplotly()


# Compare 
g_train + g

tibble(Adjusted_R2_train,
       Adjusted_R2_test = Adjusted_R2)
