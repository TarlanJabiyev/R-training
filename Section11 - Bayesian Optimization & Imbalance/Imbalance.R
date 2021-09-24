library(tidyverse)

#devtools::install_github("ncordon/imbalance")
library(imbalance)

df <- newthyroid1

df %>% glimpse()

df$Class %>% table() %>% prop.table()

df %>% imbalanceRatio("Class")

df$Class %>% table()

# MWMOTE - (Majority Weighted Minority Oversampling TEchnique) ----
new_mwmote <- df %>% mwmote(numInstances = 75, classAttr = "Class")
new_mwmote$Class %>% table()

df %>% 
  plotComparison(rbind(df, new_mwmote), 
                 attrs = names(df)[1:3])

# RACOG - (RApidly COnverging Gibbs) ----
new_RACOG <- racog(df, numInstances = 75, classAttr = "Class")
new_RACOG$Class %>% table()

df %>% 
  plotComparison(rbind(df, new_RACOG), 
                 attrs = names(df)[1:3])

# RWO - (Random Walk Oversampling) ----
new_RWO <- df %>% mwmote(numInstances = 75, classAttr = "Class")
new_RWO$Class %>% table()

df %>% 
  plotComparison(rbind(df, new_RWO), 
                 attrs = names(df)[1:3])

# PDFOS - (Probability Distribution density Function estimation based OverSampling) ----
new_PDFOS <- df %>% mwmote(numInstances = 75, classAttr = "Class")
new_PDFOS$Class %>% table()

df %>% 
  plotComparison(rbind(df, new_PDFOS), 
                 attrs = names(df)[1:3])

# NEATER - (filteriNg of oversampled data using non cooperative game theory) ----
filtered <- df %>% neater(newSamples = new_PDFOS, iterations = 500, classAttr = "Class")
filtered$Class %>% table()

df %>% 
  plotComparison(rbind(df, filtered), 
                 attrs = names(df)[1:3])

# oversample 
filtered2 <- df %>% oversample(ratio = 1, method = "PDFOS", filtering = TRUE, iterations = 500, classAttr = "Class")
filtered2$Class %>% table()

df %>% 
  plotComparison(filtered2, 
                 attrs = names(df)[1:3])
