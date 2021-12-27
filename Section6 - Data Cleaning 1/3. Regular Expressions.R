library(tidyverse)

files <- c("tmp-project.csv", "project.csv", "project2-csv-specs.csv", 
           "project2.csv2.specs.xlsx", "project_cars.ods", 
           "project-houses.csv", "Project_Trees.csv", "project-cars.R", 
           "project-houses.r", "project-final.xls", "Project-final2.xlsx")

files %>% str_detect("project")
files[files %>% str_detect("project")]

files %>% str_subset("\.csv")
files %>% str_subset("\\.csv")

files %>% str_subset("^Proj")

files %>% str_subset("\\.csv$")

files %>% str_subset("\\.ods$")
  
files %>% str_subset("\\.csv$|\\.ods$")
files %>% str_subset("\\.(csv|ods)$")
  
files %>% str_extract("[a-z]$")
files %>% str_extract("[a-zA-Z]$")
  
files %>% str_extract("[a-zA-Z]*$")

files %>% str_subset("[a-zA-Z]*\\.(csv|ods)$")

files %>% str_subset("(\\_|\\-)[a-zA-Z]*\\.(csv|ods)$")

files %>% str_subset("(P|p)roject(\\_|\\-)[a-zA-Z]*\\.(csv|ods)$")

files %>% str_subset("^(P|p)roject(\\_|\\-)[a-zA-Z]*\\.(csv|ods)$")
