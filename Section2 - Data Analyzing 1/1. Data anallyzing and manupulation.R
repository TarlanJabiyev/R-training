# Data Analyzing and Manipulation

library(tidyverse)

data <- nycflights13::flights
# "::" - to use a file or function without activating the package


data %>% str()


data %>% glimpse()


data %>% nrow() ; data %>% ncol() ; data %>% dim()


data[data %>% complete.cases(),] %>% dim()


data$carrier %>% unique() %>% length()


data$carrier %>% table()


data[1:7,1:3] %>% t()


filter(data, month %in% c(11, 12))

data %>% filter(month %in% c(11, 12))

data %>% filter(!(arr_delay>120), !(dep_delay>120))


data %>% arrange(arr_delay)

data %>% arrange(desc(arr_delay))


data %>% select(year, month, day)
data[,c("year", "month", "day")]

data %>% select(year:day)

data %>% select(!(year:day))

data %>% select(time_hour, everything(), -year)

data %>% select(contains('time'), 
                ends_with("delay"), 
                starts_with("arr"))


data %>% relocate(day, .after = flight)

data %>% relocate(flight, .after = last_col())

data %>% relocate(flight, .after = last_col(offset = 1))

data %>% relocate(starts_with("d"), .before = month)


data %>% 
  filter(grepl("N77",tailnum)) %>% 
  select(tailnum)


data %>% rename(ay = month)


data %>% count(carrier,dest) 


data %>% drop_na() %>% dim()


data %>% mutate(
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours)

data[1:5,1:6] %>% 
  mutate(countries = c('Turkey_France','Baku_Turkey','USA_France','Canada_Baku','Turkey_Baku'))

data[1:5,1:6] %>% 
  mutate(countries = c('Turkey_France','Baku_Turkey','USA_France','Canada_Baku','Turkey_Baku')) %>% 
  separate(countries,c('origin','destination'),sep = '_')

data %>% 
  mutate(c = case_when(
    .$carrier %in% c("AA","AS") ~ "A",
    .$carrier %in% c("UA","US") ~ "U",
    .$origin == "EWR" & .$carrier == "EV" ~ "E",
    .$origin == "LGA" & .$carrier != "OO" ~ "L",
    .$origin == "JFK" & !.$carrier %in% c("9E","F9")  ~ "J"
  )) %>% View()


data %>% transmute(
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours)


data %>% summarise(delay = mean(dep_delay, na.rm = TRUE))

data %>% 
  group_by(carrier,dest) %>%
  summarise(flight = mean(flight,na.rm = T)) %>% 
  arrange(desc(flight))


data %>% 
  group_by(carrier,dest) %>%
  summarise(count = n(),
            dist = mean(distance, na.rm = T),
            delay = mean(arr_delay, na.rm = T))


data %>% 
  group_by(carrier) %>%
  summarise(flight = mean(flight, na.rm = T)) %>% 
  arrange(desc(flight)) %>% 
  head(10) %>% 
  filter(carrier == "OO") 

library(ViewPipeSteps)
# Addings - View Pipe Chain Steps


data %>% 
  group_by(carrier,dest) %>% 
  tally()


data %>%
  group_by(carrier) %>%
  summarise(
    across(flight, .fns = list(mean = mean, stdev = sd)), .groups = "drop")

data %>%
  group_by(carrier) %>%
  summarise(
    across(c(flight, distance), .fns = list(mean = mean, stdev = sd)), .groups = "drop")


data[1:3,1:3] %>% 
  gather(key=variables, value=values, -year) %>% 
  as_tibble()


data %>%
  map_df(~ sum(is.na(.))) %>%
  gather(key = "variables", value = "missing_count") %>%
  arrange(desc(missing_count))


# https://tidydatatutor.com/
# https://swcarpentry.github.io/r-novice-gapminder/13-dplyr/
