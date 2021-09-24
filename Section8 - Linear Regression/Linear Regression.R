# Import libraries & dataset ----
library(tidyverse) 
library(data.table)
library(rstudioapi)
library(inspectdf)
library(mice)
library(recipes) 
library(caret) 
library(Metrics)
library(plotly)
library(glue)
library(patchwork)

# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }}
# install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yates/5/R")
library(h2o)  

path <- dirname(getSourceEditorContext()$path)
setwd(path)

raw <- fread("Life Expantancy Data.csv")

raw %>% glimpse()

raw %>% inspect_na()

raw$Year <- raw$Year %>% as_factor()

names(raw) <- names(raw) %>% 
  str_replace_all(" ","_") %>% 
  str_replace_all("-","_") %>% 
  str_replace_all("/","_")

# ----------------------------- Data Preprocessing -----------------------------

# Fill NA's ----
raw %>% 
  inspect_na() %>% 
  filter(pcnt < 40) %>% 
  pull(col_name) -> variables

raw <- raw %>% select(all_of(variables))


df.num <- raw %>%
  select_if(is.numeric) %>%
  select(Life_expectancy,everything())

df.chr <- raw %>%
  mutate_if(is.factor,as.character) %>% 
  select_if(is.character)


df.num %>% inspect_na()

df.num_mice <- df.num %>% mice(m = 5, maxit = 5 ,method='rf', seed=123)
df.num <- df.num_mice %>% complete()


df.chr %>% inspect_na()

# rec_obj <- recipe(~ ., data = df.chr) %>%
#   step_impute_mode(all_nominal()) %>%
#   prep(stringsAsFactors = FALSE)
# 
# df.chr <- bake(rec_obj, df.chr)


# One Hote Encoding ----
df.chr <- dummyVars(" ~ .", data = df.chr) %>% 
  predict(newdata = df.chr) %>% 
  as.data.frame()

df <- cbind(df.chr,df.num) %>%
  select(Life_expectancy,everything())


names(df) <- names(df) %>% 
  str_replace_all(" ","_") %>%
  str_replace_all("-","_") %>%
  str_replace_all("\\(","") %>% 
  str_replace_all("\\)","") %>% 
  str_replace_all("\\'","")


# ----------------------------- Multicollinearity -----------------------------

target <- 'Life_expectancy'
features <- df %>% select(-Life_expectancy) %>% names()

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)

glm %>% summary()

coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)

glm %>% summary()


# VIF (Variance Inflation Factor) ----
while(glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 2){
  afterVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = df)
}

glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> features 

df <- df %>% select(Life_expectancy,all_of(features))


# Standardize (Normalize) ----
df %>% glimpse()

df[,-1] <- df[,-1] %>% scale() %>% as.data.frame()


# --------------------------------- Modeling ----------------------------------
h2o.init()

h2o_data <- df %>% as.h2o()


# Splitting the data ----
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'Life_expectancy'
features <- df %>% select(-Life_expectancy) %>% names()


# Fitting h2o model ----
model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

# Significance levels of P_value:
# 0    <= p_val < 0.001 ***
# 0.001 < p_val < 0.05  **
# 0.05  < p_val < 0.01  *
# 0.01  < p_val < 0.1   .
  
  
# Stepwise Backward Elimination ----
while(model@model$coefficients_table %>%
      as.data.frame() %>%
      dplyr::select(names,p_value) %>%
      mutate(p_value = round(p_value,3)) %>%
      .[-1,] %>%
      arrange(desc(p_value)) %>%
      .[1,2] > 0.05) {
  model@model$coefficients_table %>%
    as.data.frame() %>%
    dplyr::select(names,p_value) %>%
    mutate(p_value = round(p_value,3)) %>%
    filter(!is.nan(p_value)) %>%
    .[-1,] %>%
    arrange(desc(p_value)) %>%
    .[1,1] -> v
  features <- features[features!=v]
  
  train_h2o <- train %>% as.data.frame() %>% select(target,all_of(features)) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>% select(target,all_of(features)) %>% as.h2o()
  
  model <- h2o.glm(
    x = features, y = target,
    training_frame = train,
    validation_frame = test,
    nfolds = 10, seed = 123,
    lambda = 0, compute_p_values = T)
}
model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) 


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


# Plotting actual & pred ----
my_data <- cbind(pred,actual) %>% 
  as.data.frame()

Adjusted_R2 <- eval_sum$adj.r.squared

g <- my_data %>% 
  ggplot(aes(pred, actual)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = lm) + 
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
