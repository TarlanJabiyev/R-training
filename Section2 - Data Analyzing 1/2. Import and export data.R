# Import libraries & dataset ----
library(tidyverse)
library(dslabs)
library(DataEditR)
library(rstudioapi)
library(writexl) 
library(readxl)
library(fs)


# Edit dataset with DataEditR ----
data <- gapminder %>% data_edit()


# Export dataset ----
path <- dirname(getSourceEditorContext()$path)
setwd(path)

data %>% write_csv('data/gapminder.csv')
data %>% write_xlsx('data/gapminder.xlsx')


# Import dataset ----
df_csv <- read_csv("data/gapminder.csv")
df_xlsx <- read_xlsx("data/gapminder.xlsx")


# Write each splited group ----
data %>%
  group_by(continent) %>%
  group_split() %>%
  walk(function(x) {
    write_csv(x, path = str_c("./data/", unique(x$continent), ".csv"))
  })


# Read multiple files ----
file_paths <- dir_ls("data")

file_contents <- list()

for (i in seq_along(file_paths)) {
  file_contents[[i]] <- file_paths[[i]] %>% 
    read_csv()
}

file_contents <- file_contents %>% set_names(file_paths)
