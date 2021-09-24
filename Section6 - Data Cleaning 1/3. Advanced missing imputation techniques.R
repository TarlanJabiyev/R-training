library(tidyverse)
library(dslabs)
library(inspectdf)

raw_df <- gapminder

raw_df %>% inspect_na() %>% show_plot()


# mice method
library(mice)

raw_df_mice <- raw_df %>% mice(method='rf', seed=123)
df <- raw_df_mice %>% complete()

df %>% inspect_na()


# random forest method
library(missForest)

raw_df %>% glimpse()

raw_df_rf <- raw_df %>% 
  select_if(is.numeric) %>% 
  missForest()

df <- raw_df %>% 
  select_if(is.factor) %>% 
  cbind(raw_df_rf$ximp)

df %>% inspect_na()


# simputation method
library(simputation)

raw_df %>%
  impute_lm(gdp ~ life_expectancy + fertility)

df <- raw_df %>%
  select_if(is.numeric) %>% 
  impute_rf(gdp ~ year + life_expectancy) %>%
  impute_rf(fertility ~ gdp + life_expectancy) %>%
  impute_rf(infant_mortality ~ life_expectancy + gdp) %>%
  impute_rf(population ~ infant_mortality + gdp) 

df %>% inspect_na()
