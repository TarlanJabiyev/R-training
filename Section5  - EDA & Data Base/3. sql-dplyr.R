library(tidyverse)
library(dslabs)
library(dbplyr)

db_gapminder <- gapminder %>% tbl_memdb(name = "db_gapminder")

db_gapminder %>% 
  select(country,life_expectancy,gdp) %>%
  rename(life.exp=life_expectancy) %>% 
  filter(!is.na(gdp)) %>% 
  group_by(country) %>%
  summarise(life_expectancy=max(life.exp, na.rm=T)) %>% 
  show_query()


library(tidyquery)
library(nycflights13)

show_dplyr("SELECT Species, COUNT(*) AS n FROM iris GROUP BY Species")

show_dplyr("SELECT origin,dest,   
            COUNT(*) AS num_flights,
            SUM(seats) AS num_seats
           FROM flights f LEFT JOIN planes p
            ON f.tailnum = p.tailnum
           WHERE distance BETWEEN 250 AND 450
           GROUP BY origin,dest
           HAVING num_flights > 200
           ORDER BY num_seats DESC
           LIMIT 8")
