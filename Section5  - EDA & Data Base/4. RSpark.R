library(tidyverse)
library(DBI)
library(sparklyr)

# Spark Connection
spark_install()
sc <- spark_connect(master = "local")

# Moving the dataset to Spark 
sc %>% copy_to(nycflights13::flights, "flights_spark")

sc %>% src_tbls()


# Run SQL queries here 

sc %>% dbGetQuery("show databases")

sc %>% dbGetQuery("show tables")

sc %>% dbGetQuery("select year, month, day 
                  from flights_spark 
                  where month = 2 and day = 18")

sc %>% dbGetQuery("select count(*) as n 
                  from flights_spark 
                  where month = 2 and day = 18")

sc %>% dbGetQuery("select carrier, count(*) as n, avg(dep_delay) as delay 
                  from flights_spark 
                  where month = 2 and day = 18 
                  group by carrier 
                  order by delay DESC")


# Big Data Visualization

f_tbl <- sc %>% copy_to(nycflights13::flights, 
                        "flights_spark", overwrite = T)
f_tbl

data <- f_tbl %>% 
  filter(carrier %in% c("EV","YV","B6","UA","AA")) %>%
  mutate(deppdelay = dep_delay/60) %>%
  select(carrier, deppdelay) %>% 
  collect()

data %>% 
  ggplot(aes(carrier, deppdelay)) + 
  geom_boxplot()


# Machine Learning

iris_tbl <- sc %>% copy_to(iris, "iris", overwrite = T)
iris_tbl

lm_model <- iris_tbl %>%
  select(Petal_Width, Petal_Length) %>%
  ml_linear_regression(Petal_Length ~ Petal_Width)

lm_model %>% summary()


iris_tbl %>%
  select(Petal_Width, Petal_Length) %>%
  collect %>%
  ggplot(aes(Petal_Length, Petal_Width)) +
  geom_point(aes(Petal_Width, Petal_Length), size = 2, alpha = 0.5) +
  geom_abline(aes(slope = coef(lm_model)[["Petal_Width"]],
                  intercept = coef(lm_model)[["(Intercept)"]]),
              color = "red") +
  labs(
    x = "Petal Width",
    y = "Petal Length",
    title = "Linear Regression: Petal Length ~ Petal Width",
    subtitle = "SparkML - Regression Model"
  )
