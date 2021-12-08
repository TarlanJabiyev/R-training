library(dplyr)
library(readxl)
library(janitor)
library(here)


# Cleaning dirty data

roster_raw <- read_excel(here("/Users/tarlanjabiyev/Downloads/dirty_data.xlsx")) 

roster_raw_cleaner <- read_excel(here("/Users/tarlanjabiyev/Downloads/dirty_data.xlsx"), 
                                 skip = 1,
                                 .name_repair = make_clean_names)

roster_raw <- roster_raw %>%
  row_to_names(row_number = 1) %>%
  clean_names()

roster <- roster_raw %>%
  remove_empty(c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F) %>% 
  mutate(hire_date = convert_to_date(hire_date, 
                                     character_fun = lubridate::mdy),
         cert = coalesce(certification, certification_2)) %>%
  select(-certification, -certification_2)


# Examining dirty data

roster %>% get_dupes(contains("name"))

roster %>%
  tabyl(subject)

roster %>%
  filter(hire_date > as.Date("1950-01-01")) %>%
  tabyl(employee_status, full_time)

roster %>%
  tabyl(full_time, subject, employee_status, show_missing_levels = F)

roster %>%
  tabyl(employee_status, full_time) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title("combined")

