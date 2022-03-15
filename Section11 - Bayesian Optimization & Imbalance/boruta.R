library(tidyverse)
library(Boruta)

df <- read_csv("Churn_Modelling.csv")

df <- df %>% select(-RowNumber, -CustomerId, -Surname)


set.seed(123)
boruta <- Boruta(Exited ~ ., data = df, doTrace = 2) %>% 
  TentativeRoughFix()
boruta


boruta$finalDecision[boruta$finalDecision == "Confirmed"]


boruta %>% plot(xlab = "", xaxt = "n")
list <- list()
for (i in colnames(boruta$ImpHistory)) 
  list[[i]] <- boruta$ImpHistory[is.finite(boruta$ImpHistory[,i]),i]
features <- list %>% sapply(median) %>% sort()
axis(side = 1, las = 2, labels = names(features),
     at = 1:ncol(boruta$ImpHistory), cex.axis = 0.7)



boruta %>% getSelectedAttributes()


boruta.df <- boruta %>% attStats()
boruta.df
boruta.df %>% 
  filter(decision == "Confirmed") %>% 
  nrow()



