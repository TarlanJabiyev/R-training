library(tidyverse)
library(rstudioapi)
library(arules)
library(arulesViz)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

data <- read_csv('Market_Basket_Optimisation.csv',col_names = F)


# Apriori ----
df <- read.transactions('Market_Basket_Optimisation.csv',
                        sep = ',', rm.duplicates = T)

df %>% summary()

df %>% itemFrequencyPlot(topN = 12, type = "absolute")

# Training Apriori on the df
rules <- df %>% apriori(parameter = list(support = 0.01, confidence = 0.2))


# Visualising the results ----

#lhs - left hand side ; rhs - right hand side 
rules %>% inspect() # 164 rules

rules %>% sort(by = 'support') %>% .[1:12] %>% inspect() #Bir elaqenin butun alısverisler icinde hansi nisbetde tekrarlandıgını bildirir
rules %>% sort(by = 'confidence') %>% .[1:12] %>% inspect() #X mehsulunu alan musterilerin Y mehsulunu alma ehtimalini bildirir
rules %>% sort(by = 'lift') %>% .[1:12] %>% inspect() #Elaqenin qeribeliliyini/ferqliliyini bildirir

rules %>% sort(by = 'support') %>% .[1:12] %>% plotly_arules()

rules %>% sort(by = 'support') %>% .[1:12] %>% 
  plot(method = "graph",  engine = "htmlwidget")

rules %>% ruleExplorer()
