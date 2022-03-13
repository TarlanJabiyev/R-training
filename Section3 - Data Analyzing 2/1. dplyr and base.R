library(dplyr)

mtcars <- mtcars %>% as_tibble()
iris <- iris %>% as_tibble()


# arrange(): Arrange rows by variables ----

# dplyr
mtcars %>% arrange(cyl, disp)

mtcars %>% arrange(desc(cyl), desc(disp))

# base
mtcars[order(mtcars$cyl, mtcars$disp),] 

mtcars[order(mtcars$cyl, mtcars$disp, decreasing = TRUE), ]
mtcars[order(-mtcars$cyl, -mtcars$disp), ]


# distinct(): Select distinct/unique rows ----

df <- tibble(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)

# dplyr
df %>% distinct(x) # selected columns
df %>% distinct(x, .keep_all = TRUE) # whole data frame

# base
unique(df["x"]) # selected columns
df[!duplicated(df$x), ] # whole data frame


# filter(): Return rows with matching conditions ----

# dplyr
starwars %>% filter(species == "Human")
starwars %>% filter(mass > 1000)
starwars %>% filter(hair_color == "none" & eye_color == "black")

# base 
subset(starwars, species == "Human")
subset(starwars, mass > 1000)
subset(starwars, hair_color == "none" & eye_color == "black")

# base 
starwars[which(starwars$species == "Human"), ]
starwars[which(starwars$mass > 1000), ]
starwars[which(starwars$hair_color == "none" & starwars$eye_color == "black"), ]


# mutate(): Create or transform variables ----

# dplyr
df %>% 
  mutate(z = x + y, 
         z2 = z ^ 2)

# base
df %>% 
  transform(z = x + y, 
            z2 = (x + y) ^ 2) 

# base
mtcars$cyl2 <- mtcars$cyl * 2
mtcars$cyl4 <- mtcars$cyl2 * 2


# dplyr
gf <- tibble(g = c(1, 1, 2, 2), 
             x = c(0.5, 1.5, 2.5, 3.5))
gf %>% 
  group_by(g) %>% 
  mutate(x_mean = mean(x), 
         x_rank = rank(x))

# base
transform(gf, 
          x_mean = ave(x, g, FUN = mean), 
          x_rank = ave(x, g, FUN = rank))


# pull(): Pull out a single variable ----

# dplyr
mtcars %>% pull(1)
mtcars %>% pull(cyl)

# base
mtcars[[1]]
mtcars$cyl


# relocate(): Change column order ----

# dplyr 
mtcars %>% relocate(gear, carb) # to front
mtcars %>% relocate(mpg, cyl, .after = last_col()) # to back

# base
mtcars[union(c("gear", "carb"), names(mtcars))] # to front
mtcars[c(setdiff(names(mtcars), c("mpg", "cyl")), c("mpg", "cyl"))] # to back


# rename(): Rename variables by name ----

# dplyr
iris %>% rename(sepal_length = Sepal.Length, sepal_width = 2)

# base
iris2 <- iris
names(iris2)[2] <- "sepal_width"

# base
names(iris2)[names(iris2) == "Sepal.Length"] <- "sepal_length"


# rename_with(): Rename variables with a function ----

# dplyr
iris %>% rename_with(toupper)

# base
setNames(iris, toupper(names(iris)))


# select(): Select variables by name ----

# dplyr 
iris %>% select(1:3)
iris %>% select(Species, Sepal.Length)
iris %>% select(starts_with("Petal"))
iris %>% select(where(is.factor))

# base
iris[1:3] 
iris[1:3, ]

# base
iris[c("Species", "Sepal.Length")]
subset(iris, select = c(Species, Sepal.Length))

# base
iris[grep("^Petal", names(iris))]


# summarise(): Reduce multiple values down to a single value ----

# dplyr
mtcars %>% 
  group_by(cyl) %>% 
  summarise(mean = mean(disp), 
            n = n())

# base
mtcars_by <- by(mtcars, mtcars$cyl, function(df) {
  with(df, data.frame(cyl = cyl[[1]], 
                      mean = mean(disp), 
                      n = nrow(df)))
})
do.call(rbind, mtcars_by)

# stats
aggregate(disp ~ cyl, 
          mtcars, 
          function(x) c(mean = mean(x), 
                        n = length(x)))


# slice(): Choose rows by position ----

# dplyr
mtcars %>% slice(25:n())

# base
mtcars[25:nrow(mtcars), ]


# Join / Merge ----

inner_join(df1, df2) # dplyr
merge(df1, df2) # base

left_join(df1, df2) # dplyr
merge(df1, df2, all.x = TRUE) # base

right_join(df1, df2) # dplyr
merge(df1, df2, all.y = TRUE) # base

full_join(df1, df2)	# dplyr
merge(df1, df2, all = TRUE) # base

semi_join(df1, df2)	# dplyr
df1[df1$x %in% df2$x, ] # base

anti_join(df1, df2) # dplyr
df1[!df1$x %in% df2$x, ] # base

