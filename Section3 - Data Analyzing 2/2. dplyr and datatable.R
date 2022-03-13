# Required libraries ----

library(tidyverse)
library(data.table)

# Create a data frame (tibble)
df <- tibble(C1 = rep(c(1L, 2L), 5)[-10],
             C2 = 1:9,
             C3 = rep(c(0.5, 1.0, 1.5), 3),
             C4 = rep(LETTERS[1:3], 3))
df %>% class()
df

# Create a data table
dt <- data.table(C1 = rep(c(1L, 2L), 5)[-10],
                 C2 = 1:9,
                 C3 = c(0.5, 1.0, 1.5),
                 C4 = rep(LETTERS[1:3], 3))
class(dt)
dt


# Filter rows ----

# Filter rows using indices

df[3:4,]
df %>% slice(3:4) 

dt[3:4,]
dt[3:4]

# Discard rows using negative indices

df[-(3:7),]
df %>% slice(-(3:7)) 

dt[!3:7,]
dt[-(3:7)] 

#Filter rows using a logical expression

df %>% filter(C2 > 5)
df %>% filter(C4 %in% c("A", "C"))

dt[C2 > 5]
dt[C4 %chin% c("A", "C")] #fast %in% for character

# Filter rows using multiple conditions

df %>% filter(C1 == 1, C4 == "A")
dt[C1 == 1 & C4 == "A"]

# Filter unique rows

df %>% distinct() 
unique(dt)

df %>% distinct(across(c(C1, C4))) #returns selected cols
unique(dt, by = c("C1", "C4")) #returns all cols

# Discard rows with missing values

df %>% drop_na(names(df))
na.omit(dt, cols = 1:4) 

# Other filters

df %>% slice_sample(n = 3) #df %>% sample_n(3)
dt[sample(.N, 3)] 

df %>% slice_sample(prop = 0.5) #df %>% sample_frac(0.5)
dt[sample(.N, .N / 2)]

df %>% slice_max(C1, n = 1) #df %>% top_n(1, C1)
dt[frankv(-C1, ties.method = "dense") < 2]

df %>% filter(grepl("^B", C4))
dt[C4 %like% "^B"]

df %>% filter(dplyr::between(C2, 3, 5))
dt[C2 %between% c(3, 5)]

df %>% filter(C2 > 3 & C2 < 5)
dt[data.table::between(C2, 3, 5, incbounds = F)]


# Sort rows ----

# Sort rows by column

df %>% arrange(C3)
dt[order(C3)]  

# Sort rows in decreasing order

df %>% arrange(desc(C3))
dt[order(-C3)]

# Sort rows based on several columns

df %>% arrange(C1, desc(C2))
dt[order(C1, -C2)]


# Select columns ----

# Select one column using an index (not recommended)

dt[[3]] #returns a vector
df[[3]] #returns a vector

dt[, 3] #returns a data.table
df[3]   #returns a tibble

# Select one column using column name

df %>% select(C2) 
df[, "C2"] 
dt[, .(C2)] #dt[, list(C2)]
dt[, "C2"]    

df %>% pull(C2) 
df[["C2"]] 
dt[, C2]     
dt[["C2"]]  

# Select several columns

df %>% select(C2, C3, C4)
df %>% select(C2:C4) 

dt[, .(C2, C3, C4)] #dt[, list(C2, C3, C4)]
dt[, C2:C4] 

# Exclude columns

df %>% select(-C2, -C3)
dt[, !c("C2", "C3")]

# Select/Exclude columns using a character vector

cols <- c("C2", "C3")
df %>% select(!!cols) 
df %>% select(-!!cols)

cols <- c("C2", "C3")
dt[, ..cols] 
dt[, !..cols] #dt[, -..cols]

# Other selections

df %>% select(num_range("C", 1:2))
cols <- paste0("C", 1:2);dt[, ..cols]

df %>% select(C4, everything())
cols <- union("C4", names(dt));dt[, ..cols]

df %>% select(contains("C"))
cols <- grep("C",   names(dt));dt[, ..cols]

df %>% select(ends_with("3"))
cols <- grep("3$",  names(dt));dt[, ..cols]

df %>% select(matches(".2"))
cols <- grep(".2",  names(dt));dt[, ..cols]

df %>% select(one_of(c("C1", "X")))
cols <- grep("^C1|X$",  names(dt));dt[, ..cols]

df %>% select(-starts_with("C2"))
cols <- grep("^(?!C2)", names(dt), perl=T);dt[, ..cols]


# Summarise data ----

# Summarise one column

df %>% summarise(sum(C1)) #returns a tibble
dt[, sum(C1)]    #returns a vector
dt[, .(sum(C1))] #returns a data.table

df %>% summarise(sumC1 = sum(C1)) #returns a tibble
dt[, .(sumC1 = sum(C1))] #returns a data.table

# Summarise several columns

df %>% summarise(sum(C1), sd(C3))
dt[, .(sum(C1), sd(C3))]

# Summarise several columns and assign column names

df %>%
  summarise(sumC1 = sum(C1),
            sdC3  = sd(C3))
dt[, .(sumC1 = sum(C1),
       sdC3  = sd(C3))]

# Summarise a subset of rows

df %>%
  slice(1:4) %>%
  summarise(sum(C1))
dt[1:4, sum(C1)]

df %>% summarise(dplyr::first(C3))
dt[, data.table::first(C3)]

df %>% summarise(dplyr::last(C3))
dt[, data.table::last(C3)]

df %>% summarise(nth(C3, 5))
dt[5, C3]

df %>% summarise(n_distinct(C4))
dt[, uniqueN(C4)]

df %>% n_distinct()
uniqueN(dt)


# Add/update/delete columns ----

# Modify a column

df <- df %>% mutate(C1 = C1^2)
df
dt[, C1 := C1^2]
dt

# Add one column

df <- df %>% mutate(v5 = log(C1))
df
dt[, v5 := log(C1)][] 

# Add several columns

df <- df %>% mutate(v6 = sqrt(C1), v7 = "X")
df
dt[, c("v6", "v7") := .(sqrt(C1), "X")]
dt
# dt[, ':='(v6 = sqrt(C1), 
#           v7 = "X")] #same, functional form


# Create one column and remove the others

df %>% transmute(v8 = C3 + 1)
dt[, .(v8 = C3 + 1)]

# Remove one column

df <- df %>% select(-v5)
df
dt[, v5 := NULL]
dt

# Remove several columns

df <- df %>% select(-v6, -v7)
df
dt[, c("v6", "v7") := NULL]
dt

# Remove columns using a vector of colnames

df <- df %>% select(-one_of(c("C3")))
df
dt[, (c("C3")) := NULL] 
dt

# Replace values for rows matching a condition

df <- df %>% mutate(C2 = replace(C2, C2 < 4, 0L))
df
dt[C2 < 4, C2 := 0L]
dt


# by ----

# By group

df %>%
  group_by(C4) %>%
  summarise(sumC2 = sum(C2))

dt[,.(sumC2 = sum(C2)), 
   by = "C4"]
dt[,by = C4,
   .(sumC2 = sum(C2))]

# By several groups

df %>%
  group_by(C4, C1) %>%
  summarise(sumC2 = sum(C2)) 

dt[, keyby = .(C4, C1),
   .(sumC2 = sum(C2))]

# Calling function in by

df %>%
  group_by(tolower(C4)) %>%
  summarise(sumC1 = sum(C1))

dt[, by = tolower(C4),
   .(sumC1 = sum(C1))]

# Assigning column name in by

df %>%
  group_by(abc = tolower(C4)) %>%
  summarise(sumC1 = sum(C1))

dt[, keyby = .(abc = tolower(C4)),
   .(sumC1 = sum(C1))]

# Using a condition in by

df %>%
  group_by(C4 == "A") %>%
  summarise(sum(C1))

dt[, keyby = (C4 == "A"),
   sum(C1)]

# By on a subset of rows

df %>%
  slice(1:5) %>%
  group_by(C4) %>%
  summarise(sumC1 = sum(C1))

dt[1:5,       
   .(sumC1 = sum(C1)), 
   by = C4]            

# Count number of observations for each group

df %>% count(C4)
df %>%
  group_by(C4) %>%
  tally()
df %>%
  group_by(C4) %>%
  summarise(n())
df %>%
  group_by(C4) %>%
  group_size() #returns a vector

dt[, .N, by = C4]

# Add a column with number of observations for each group

df %>% add_count(C1)
df %>%
  group_by(C1) %>%
  add_tally()

dt[, n := .N, by = C1][]

# Retrieve the first/last/nth observation for each group

df %>%
  group_by(C4) %>%
  summarise(dplyr::first(C2))
dt[, data.table::first(C2), by = C4]

df %>%
  group_by(C4) %>%
  summarise(dplyr::last(C2))
dt[, data.table::last(C2), by = C4]

df %>%
  group_by(C4) %>%
  summarise(dplyr::nth(C2, 2))
dt[, C2[2], by = C4]


# Advanced columns manipulation ----

# Summarise all the columns

df %>% summarise(across(everything(), max)) #df %>% summarise_all(max)
dt[, lapply(.SD, max)]

# Summarise several columns

df %>% summarise(across(c(C1, C2), mean)) #df %>% summarise_at(c("C1", "C2"), mean)
dt[, lapply(.SD, mean),
   .SDcols = c("C1", "C2")]

# Summarise several columns by group

df %>%
  group_by(C4) %>%
  summarise(across(c(C1, C2), mean))
# df %>%
#   group_by(C4) %>%
#   summarise_at(c("C1", "C2"), mean)
df %>%
  group_by(C4) %>%
  summarise(across(any_of(c("C1", "C2", "Z0")), mean))

dt[, by = C4,
   lapply(.SD, mean),
   .SDcols = c("C1", "C2")]
dt[, by = C4,
   lapply(.SD, mean),
   .SDcols = patterns("C1|C2|Z0")]

# Summarise with more than one function by group

df %>%
  group_by(C4) %>%
  summarise(across(everything(),
                   list(sum = sum, mean = mean))) #columns named automatically
# df %>%
#   group_by(C4) %>%
#   summarise_all(list(sum, mean))

dt[, by = C4,
   c(lapply(.SD, sum),
     lapply(.SD, mean))]

# Modify several columns (keeping the others)

df <- df %>%
  mutate(across(all_of(c("C1", "C2")), sqrt)) 
#df %>% mutate_at(c("C1", "C2"), sqrt)
df
dt[, c("C1", "C2") := lapply(.SD, sqrt),
   .SDcols = c("C1", "C2")]
dt

df <- df %>%
  mutate(across(-any_of("C4"),
                ~ "^"(.x, 2)))
#df %>% mutate_at(vars(-C4), "^", 2L)
df
cols <- setdiff(names(dt), "C4")
dt[, (cols) := lapply(.SD, "^", 2L),
   .SDcols = cols]
dt


# Indexing and Keys ----

# Set the key/index

df <- df %>% arrange(C4) 
df

setkey(dt, C4)
setindex(dt, C4)
dt

# Select the matching rows

df %>% filter(C4 == "A")
dt["A", on = "C4"]

df %>% filter(C4 %in% c("A", "C"))
dt[c("A", "C"), on = .(C4)] 

# Select the first matching row

df %>%
  filter(C4 == "B") %>%
  slice(1)

dt["B", on = "C4", mult = "first"]
dt[c("B", "C"), on = "C4", mult = "first"]

# Select the last matching row

df %>%
  filter(C4 == "A") %>%
  slice(n())

dt["A", on = "C4", mult = "last"]

# Apply a function on the matching rows

df %>%
  filter(C4 %in% c("A", "C")) %>%
  summarise(sum(C1))

dt[c("A", "C"), sum(C1), on = "C4"]

# Modify values for matching rows

df <- df %>%
  mutate(C1 = replace(C1, C4 == "A", 0L)) %>%
  arrange(C4)
df

dt["A", C1 := 0, on = "C4"]
dt

# Use keys in by

df %>%
  filter(C4 != "B") %>%
  group_by(C4) %>%
  summarise(sum(C1))

dt[!"B", sum(C1), on = "C4", by = .EACHI]
dt[C4 != "B",
   by = C4,
   sum(C1)] #same

# Set keys/indices for multiple columns

df <- df %>% arrange(C4, C1)
df

setkey(dt, C4, C1) #setindex(dt, C4, C1) 
dt

# Subset using multiple keys/indices

df %>% filter(C1 == 1, C4 == "C")
dt[.("C", 1), on = .(C4, C1)]

df %>% filter(C1 == 1, C4 %in% c("B", "C"))
dt[.(c("B", "C"), 1), on = .(C4, C1)]
dt[.(c("B", "C"), 1), on = .(C4, C1), which=T] 
#using which = TRUE only returns the matching rows indices


# set*() modifications ----

# Replace values

df[1, 2] <- 3L
df
set(dt, i = 1L, j = 2L, value = 3L)
dt

# Reorder rows

df <- df %>% arrange(C4, desc(C1))
setorder(dt, C4, -C1)

# Reorder columns

df <- df %>% select(C4, C1, C2)
df
setcolorder(dt, c("C4", "C1", "C2"))
dt

# Advanced use of by ----

# Select first/last/… row by group

df %>%
  group_by(C4) %>%
  slice(1)
dt[, .SD[1], by = C4]

df %>%
  group_by(C4) %>%
  slice(1, n())
dt[, .SD[c(1, .N)], by = C4]

df %>%
  group_by(C4) %>%
  group_map(~ tail(.x, 2))
dt[, tail(.SD, 2), by = C4]

# Select rows using a nested query

df %>%
  group_by(C4) %>%
  arrange(C2) %>%
  slice(1)

dt[, .SD[which.min(C2)], by = C4]


# Reshape data ----

# Melt data (from wide to long)

df %>% gather(variable, value, -C4)
melt(dt, id.vars = "C4")

mdf <- df %>% 
  gather(key   = Variable,
         value = Value,
         -C4)
mdt <- melt(dt,
            id.vars       = "C4",
            measure.vars  = c("C1", "C2"),
            variable.name = "Variable",
            value.name    = "Value")

# Cast data (from long to wide)

spread(data  = count(mdf, C4, Variable),
       key   = Variable,
       value = n,
       fill  = 0)

dcast(mdt, C4 ~ Variable) #aggregate by count
dcast(mdt, C4 ~ Variable, fun.aggregate = sum)
dcast(mdt, C4 ~ Value > 5)

# Split

df %>% group_split(C4)
split(dt, by = "C4")

# Split and transpose a vector/column

vec <- c("A:a", "B:b", "C:c")
vec %>% as_tibble() %>% 
  separate(value, c("C1", "C2"), sep = ":")

vec <- c("A:a", "B:b", "C:c")
tstrsplit(vec, split = ":", keep = 2) 
setDT(tstrsplit(vec, split = ":"))[]


# Other ----

# Lead/Lag

lag(1:10, n = 1, default = NA)
shift(1:10, n = 1,   fill = NA, type = "lag")

map(1:2, ~lag(1:10, n = .x))
shift(1:10, n = 1:2, fill = NA, type = "lag") 

lead(1:10, n = 1, default = NA)
shift(1:10, n = 1,   fill = NA, type = "lead")

# Fast version of ifelse()

x <- c(-3:3, NA)
if_else(condition = x < 0,
        true      = "neg",
        false     = "pos",
        missing   = "NA")

x <- c(-3:3, NA)
fifelse(test = x < 0,
        yes  = "neg",
        no   = "pos",
        na   = "NA")

# Vectorised ifelse statements

x <- 1:10
case_when(
  x %% 6 == 0 ~ "fizz buzz",
  x %% 2 == 0 ~ "fizz",
  x %% 3 == 0 ~ "buzz",
  TRUE ~ as.character(x)
)

x <- 1:10
fcase(
  x %% 6 == 0, "fizz buzz",
  x %% 2 == 0, "fizz",
  x %% 3 == 0, "buzz",
  default = NA_character_
)


# Join ----

x <- data.table(Id  = c("A", "B", "C", "C"),
                X1  = c(1L, 3L, 5L, 7L),
                XY  = c("x2", "x4", "x6", "x8"),
                key = "Id")
y <- data.table(Id  = c("A", "B", "B", "D"),
                Y1  = c(1L, 3L, 5L, 7L),
                XY  = c("y1", "y3", "y5", "y7"),
                key = "Id")

# Join matching rows from y to x

y[x]
y[x, on = "Id"]

x %>% left_join(y, by = "Id")
merge(x, y, all.x = T, by = "Id")

# Join matching rows from x to y

x[y]
x[y, on = "Id"]

x %>% right_join(y, by = "Id")
merge(x, y, all.y = T, by = "Id")

# Join matching rows from both x and y

x[y, nomatch = 0]
x[y, on = "Id", nomatch = 0]

x %>% inner_join(y, by = "Id")
merge(x, y)

# Join keeping all the rows

x %>% full_join(y, by = "Id")
merge(x, y, all = T, by = "Id")

# Return rows from x matching y

x %>% semi_join(y, by = "Id")

unique(x[y$Id, on = "Id", nomatch = 0])
unique(x[y$Id, nomatch = 0]) 

# Return rows from x not matching y

x %>% anti_join(y, by = "Id")

x[!y, on = "Id"]
x[!y] 

# Bind ----

x <- data.table(1:3)
y <- data.table(4:6)
z <- data.table(7:9, 0)

# Bind rows

x %>% bind_rows(y)
rbind(x, y)

x %>% bind_rows(z) 
rbind(x, z, fill = T)

# Bind rows using a list

list(x, y) %>% bind_rows(.id = "id")
rbindlist(list(x, y), idcol = T)

# Bind columns

x %>% bind_cols(y)
cbind(x, y)


# Set operations ----

x <- data.table(c(1, 2, 2, 3, 3))
y <- data.table(c(2, 2, 3, 4, 4))

# Intersection

x %>% intersect(y)

fintersect(x, y)
fintersect(x, y, all = T)

# Difference

x %>% setdiff(y)

fsetdiff(x, y)
fsetdiff(x, y, all = T)

# Union

x %>% union(y)
funion(x, y)

x %>% union_all(y)
funion(x, y, all = T)

# Equality

x %>% setequal(x[order(-V1),])
fsetequal(x, x[order(-V1),])

x %>% all_equal(x)
all.equal(x, x)


# Read / Write data ----

# Write data to a csv file

df %>% write_csv("df.csv")
fwrite(dt, "dt.csv")

# Write data to a tab-delimited file

df %>% write_delim("df.txt", delim = "\t")
fwrite(dt, "dt.txt", sep = "\t")

# Read a csv / tab-delimited file

read_csv("df.csv")
read_delim("df.txt", delim = "\t")

fread("dt.csv")
#fread("dt.csv", verbose = TRUE) #full details
fread("dt.txt", sep = "\t")
