library(tidyverse)

data <- dslabs::gapminder

library(data.table)
d <- data %>% dcast(region ~ continent, value.var = 'gdp')


# Convert Numeric to Factor
data$l <- data$life_expectancy %>% cut(c(-Inf, 25, 50, 75, Inf))
levels(data$l) = c("(-Inf-25]", "(25-50]", "(50-75]", "(75-Inf]")


# Recode a Variable
library(car)
data$d <- data$continent %>% 
  recode("'Americas'='West';'Europe'='West';'Africa'='West';'Oceania'='East';'Asia'='East'")


# Cut a Numeric Variable into Intervals
library(Hmisc)  
data$p <- data$population %>% cut2(g=10)
data$p %>% unique()
