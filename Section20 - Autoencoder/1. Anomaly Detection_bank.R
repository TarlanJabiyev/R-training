# Anomaly Detection

# Load packages, import and prepare data ----
library(tidyverse)
library(ggridges)
library(h2o)

fraud <- read_delim('creditcard.csv',delim = ',')

fraud$Class %>% table() %>% prop.table() 

# Visualization
fraud %>%
  gather(variable, value, -Class) %>% 
  ggplot(aes(y = as.factor(variable), 
             fill = as.factor(Class), 
             x = percent_rank(value))) + 
  geom_density_ridges() +  
  labs(y = 'Variable', x = 'Percent rank') + 
  guides(fill=guide_legend(title="Class"))

fraud[sample(nrow(fraud)),] -> df 


# Modeling ----
h2o.init() 
h2o_data <- df %>% as.h2o() 

h2o_data <- h2o.splitFrame(h2o_data,ratios = c(0.8),seed=123) 
train <- h2o_data[[1]]
test <- h2o_data[[2]]

# Fitting
anomaly_model <- h2o.deeplearning(
  x = names(train), 
  training_frame = train, 
  activation = "Tanh", # aktivasiya funskiyasi
  autoencoder = T, 
  hidden = c(100,50,100), 
  sparse = T, # Eyni neyron her iki qrupu (yeni 0 ve 1-leri) eyni anda emal etmeyecek,
  # Chunki her bir neyron 1 qrupu daha yaxshi analiz ede bilir neyinki eyni anda her 2 qrupu.
  l1 = 0.0001, # Oyrenme sureti
  epochs = 100) # emeliyyati tekrarlama sayi

recon_error <- anomaly_model %>% h2o.anomaly(test) %>% as.data.frame()

recon_error %>% plot.ts()

anomaly <- recon_error %>% 
  rownames_to_column() %>% 
  mutate(Class = as.vector(test[31])) 

anomaly$Reconstruction.MSE %>% quantile(0.95)

anomaly <- anomaly %>%
  mutate(outlier = ifelse(Reconstruction.MSE > 0.02, "outlier", "no_outlier")) #{0.02 <- plotdaki serhed xet}

anomaly %>%
  group_by(Class, outlier) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  mutate(freq2 = n / nrow(anomaly)) #{0 no_outlier -> dogru pred, 0 outlier -> yanlis pred, 1 outlier -> dogru pred} confision matrix e oxsuyur

anomaly %>% 
  ggplot(aes(x = as.numeric(rowname), y = Reconstruction.MSE, 
             color = as.factor(Class))) +
  geom_point(alpha = 0.5, size=2) +
  geom_hline(aes(yintercept = 0.02), color = 'darkgreen') +
  scale_color_manual(values = c('red','blue'))+
  labs(x = "observation number",
       color = "Class") #+ facet_wrap( ~ Class) 
# plot a gore goyler anormaldi. ancag bu musteriler daha yaxsidi. yeni bu musteriler daha cox pul kocurubler. marketinge gore bu musterilere daha cox qaygi lazimdi.
