library(tidyverse)
library(shinyML)

# Regression
iris %>% 
  shinyML_regression("Petal.Width", "h2o")

# Classification
iris %>% 
  shinyML_classification("Species", "h2o")

# Time Series 
longley %>% 
  mutate(Year = Year %>% as.character() %>% as.Date("%Y")) %>% 
  shinyML_regression("Population", "h2o")
