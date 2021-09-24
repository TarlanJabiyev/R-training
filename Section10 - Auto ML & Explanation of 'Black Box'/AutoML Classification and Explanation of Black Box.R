# Import libraries & dataset ----
library(tidyverse)
library(data.table)
library(rstudioapi)
library(inspectdf)
library(car)
library(h2o)
library(rlang)
library(glue)
library(yardstick)
library(ggsci)
library(highcharter)
library(lime)


path <- dirname(getSourceEditorContext()$path)
setwd(path)

raw <- fread("bank.csv")

raw %>% glimpse()

raw %>% inspect_na()

raw$y %>% table() %>% prop.table()

raw$y <- raw$y %>% recode(" 'yes'=1 ; 'no'=0 ") %>% as_factor()


# --------------------------------- Modeling ----------------------------------
h2o.init()

h2o_data <- raw %>% as.h2o()


# Splitting the data ----
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'y'
features <- raw %>% select(-y) %>% names()


# Fitting h2o model ----
model <- h2o.automl(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  leaderboard_frame = test,
  stopping_metric = "AUC",
  nfolds = 10, seed = 123,
  max_runtime_secs = 480)

model@leaderboard %>% as.data.frame()
model@leader 


# Variable Importance ----
model@leader %>% 
  h2o.varimp() %>% 
  as.data.frame() %>% 
  .[.$percentage != 0,] %>%
  select(variable, percentage) %>%
  hchart("pie", hcaes(x = variable, y = percentage)) %>%
  hc_colors(colors = 'orange') %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)


# Predicting the Test set results ----
pred <- model@leader %>% h2o.predict(test) %>% as.data.frame()

# Threshold / Cutoff
model@leader %>% 
  h2o.performance(test) %>% 
  h2o.find_threshold_by_max_metric('f1') -> treshold


# ----------------------------- Model evaluation -----------------------------

# Confusion Matrix----
cm <- pred %>% 
  bind_cols(test %>% as.data.frame() %>% select(y)) %>% 
  conf_mat(y, predict) %>% 
  pluck(1) %>% 
  as_tibble() %>%
  mutate(true_pred = ifelse(Prediction == Truth,1,0))

cm$n[cm$true_pred == 1] %>% sum() / cm$n %>% sum() -> accuracy

cm %>% 
  as_tibble() %>% 
  ggplot(aes(x=Prediction, y=Truth, fill=n)) +
  geom_tile(show.legend=F, alpha=0.5) +
  geom_text(aes(label=n), color="black", alpha=1, size=8) + 
  labs(
    title = glue('Accuracy = {round(enexpr(accuracy),2)}%')
  ) +
  scale_fill_gsea()


# Area Under Curve (AUC) ----
# threshold - proqnozları o ve 1 e cevirmek ucun secilmmis optimal limit xetdir
# precision - tp/(tp+fp)
# recall    - tp/(tp+fn)
model@leader %>% 
  h2o.performance(test) %>% 
  h2o.metric() %>% 
  select(threshold,precision,recall,tpr,fpr) %>% 
  add_column(tpr_r=runif(nrow(.),min=0.001,max=1)) %>% 
  mutate(fpr_r=tpr_r) %>% 
  arrange(tpr_r,fpr_r) -> deep_metrics

model@leader %>% 
  h2o.performance(test) %>% 
  h2o.auc() %>% round(2) -> auc

highchart() %>% 
  hc_add_series(deep_metrics, "scatter", hcaes(y=tpr,x=fpr), color='green', name='TPR') %>%
  hc_add_series(deep_metrics, "line", hcaes(y=tpr_r,x=fpr_r), color='red', name='Random Guess') %>% 
  hc_add_annotation(
    labels = list(
      point = list(xAxis=0,yAxis=0,x=0.3,y=0.6),
      text = glue('AUC = {enexpr(auc)}'))
  ) %>%
  hc_title(text = "ROC Curve") %>% 
  hc_subtitle(text = "Model is performing much better than random guessing") 


# Check overfitting ----
model@leader %>%
  h2o.auc(train = T,
          valid = T,
          xval = T) %>%
  as_tibble() %>%
  round(2) %>%
  mutate(data = c('train','test','cross_val')) %>%
  mutate(gini = 2*value-1) %>%
  select(data,auc=value,gini)


# Explanation of 'Black Box' ----
explainer <- train %>% as.data.frame() %>% select(-y) %>% 
  lime(model@leader, bin_continuous = F)

explanation <- test[c(1:10),] %>% as.data.frame() %>% select(-y) %>% 
  explain(explainer, n_labels = 1, n_features = 6, kernel_width = 2)

explanation[19:48,] %>% plot_features() # 19:48 - 4,5,6,7,8 ci case lere baxmaq ucun

explanation %>% plot_explanations()


# Save model ----
model@leaderboard %>% as_tibble() %>% slice(1) %>% pull(model_id) %>% 
  h2o.getModel() %>% h2o.saveModel(path = path)

# Save olunan model ile yeni prediction etmek ucun save olunan modeli R a load etmek lazimdir
loaded <- h2o.loadModel(paste0(path,"/GBM_4_AutoML_20200829_110015"))
loaded %>% h2o.auc()
