# Installation ----

# devtools::install_github("rstudio/sparklyr")
# library(sparklyr)
# 
# options(timeout = 1e5); getOption('timeout')
# spark_install()
# 
# install.packages("rsparkling", type = "source",
#                  repos = "http://h2o-release.s3.amazonaws.com/sparkling-water/spark-2.1/3.32.1.7-1-2.1/R")


# Connecting to Spark ----

library(sparklyr)
sc <- spark_connect(master = "local")


# Using dplyr ----
library(tidyverse)

iris_tbl <- sc %>% 
  copy_to(iris, "iris")

flights_tbl <- sc %>%
  copy_to(nycflights13::flights, "flights")

batting_tbl <- sc %>%
  copy_to(Lahman::Batting, "batting")

sc %>% src_tbls()

# filter by departure delay and print the first few records
flights_tbl %>% 
  filter(dep_delay == 2)

delay <- flights_tbl %>%
  group_by(tailnum) %>%
  summarise(count = n(), 
            dist = mean(distance,na.rm = T), 
            delay = mean(arr_delay,na.rm = T)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect()

delay %>% 
  ggplot(aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 0.5) +
  geom_smooth() +
  scale_size_area(max_size = 2)

batting_tbl %>%
  select(playerID, yearID, teamID, G, AB:H) %>%
  arrange(playerID, yearID, teamID) %>%
  group_by(playerID) %>%
  filter(min_rank(desc(H)) <= 2 & H > 0)


# Using SQL ----
library(DBI)

sc %>% dbGetQuery("show databases")

sc %>% dbGetQuery("show tables")

flights_preview <- sc %>% dbGetQuery("SELECT year, month, day 
                                     FROM flights
                                     WHERE month = 2 AND day = 18
                                     LIMIT 10")
flights_preview

sc %>% dbGetQuery("SELECT COUNT(*) as n 
                  FROM flights 
                  WHERE month = 2 AND day = 18")

sc %>% dbGetQuery("SELECT carrier, COUNT(*) as n, AVG(dep_delay) as delay 
                  FROM flights
                  WHERE month = 2 AND day = 18 
                  GROUP BY carrier 
                  ORDER BY delay DESC")


# Machine Learning ----

mtcars_tbl <- sc %>% 
  copy_to(mtcars)

# transform our data set, and then partition into training & test
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_random_split(training = 0.7, test = 0.3, seed = 123)

# fit a linear model to the training dataset
fit <- partitions$training %>%
  ml_linear_regression(response = "mpg", features = c("wt", "cyl"))

fit

fit %>% summary()


# Distributed R ----

# group by columns to perform an operation over each group of   
# rows and make use of any package within the closure
iris_tbl %>% 
  spark_apply(
  function(e) broom::tidy(lm(Petal_Width ~ Petal_Length, e)),
  names = c("term", "estimate", "std.error", "statistic", "p.value"),
  group_by = "Species")


# Connection Utilities ----

sc %>% spark_web()

sc %>% spark_disconnect()
