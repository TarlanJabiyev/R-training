library(tidyverse)
library(rstudioapi)
library(RSQLite)

# Read Data ----
path <- dirname(getSourceEditorContext()$path)
setwd(path)

con <- dbConnect(SQLite(),"bikes_database.db")

# Get table names
con %>% dbListTables()

# Import tables
products <- con %>% tbl("bikes") %>% collect()
customers <- con %>% tbl("bikeshops") %>% collect()
orders <- con %>% tbl("orderlines") %>% collect()

# Disconnect from the database
con %>% dbDisconnect()


# Join datasets ----

products %>% glimpse()
customers %>% glimpse()
orders %>% glimpse()


# join()
joined <- left_join(orders, products, 
                    by = c("product.id" = "bike.id"))


# merge()
merged <- merge(orders, products, 
                by.x = "product.id", by.y = "bike.id")

merged <- merge(orders, products, 
                by.x = "product.id", by.y = "bike.id",
                all.x = T)
# all.x = T - left join
# all.y = T - right join
# all = F - inner join
# all = T - outer join


bike_orderlines <- orders %>%
  merge(products, by.x = "product.id", by.y = "bike.id", all.x = T) %>%
  merge(customers, by.x = "customer.id", by.y = "bikeshop.id", all.x = T)

bike_orderlines %>% glimpse()

bike_orderlines %>% write_csv("bike_orderlines.csv")


# MySQL database ----

# Connect to the MySQL database: con
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "tweater", 
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                 port = 3306,
                 user = "student",
                 password = "datacamp")

con %>% dbListTables()

comments <- con %>% tbl("comments") %>% collect()
tweats <- con %>% tbl("tweats") %>% collect()

con %>% dbDisconnect()
