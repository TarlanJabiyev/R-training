library(tidyverse)
library(dtplyr)

# tidyr translations

dt <- data.frame(x = c(NA, "x.y", "x.z", "y.z")) %>% 
  lazy_dt()

dt %>% 
  separate(x, c("A", "B"), sep = "\\.", remove = F) %>% 
  show_query()

dt <- data.frame(x = c(1, NA, NA, 2, NA)) %>% 
  lazy_dt()

dt %>% 
  fill(x) %>% 
  show_query()

dt %>% 
  replace_na(list(x = 99)) %>% 
  show_query()

dt <- relig_income %>% lazy_dt()

dt %>%
  pivot_longer(!religion, names_to = "income", values_to = "count") %>% 
  show_query()

# Improvements to joins

dt1 <- data.frame(x = 1:3) %>% 
  lazy_dt()

dt2 <- data.frame(x = 2:3, y = c("a", "b")) %>% 
  lazy_dt()

dt1 %>% 
  inner_join(dt2, by = "x") %>% 
  show_query()

dt1 %>% 
  left_join(dt2, by = "x") %>% 
  show_query()

dt2 %>% 
  right_join(dt1, by = "x") %>% 
  show_query()

