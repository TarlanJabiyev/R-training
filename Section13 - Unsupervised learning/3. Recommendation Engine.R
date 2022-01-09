# Import libraries & datasets ----
library(tidyverse)
library(qdapTools)
library(recommenderlab)

# https://files.grouplens.org/datasets/movielens
movies <- read_csv("movies.csv")
ratings <- read_csv("ratings.csv")
tags <- read_csv("tags.csv")

ratings <- ratings %>% 
  group_by(movieId) %>% 
  summarise(avg_rating = mean(rating))

tags <- tags %>% 
  select(-userId, -timestamp)

# Data Preprocessing ----

# Split genres to 1 genre per column per movie, only keep numeric values
movies_clean <- movies %>% 
  cbind(mtabulate(str_split(.$genres, "\\|"))) %>% 
  select(-title, -genres, -`(no genres listed)`)

# Add average reating to movies, filter out movies without rating
movies_rated <- movies_clean %>% 
  inner_join(ratings,
             by = "movieId")

# Prepare dataset for recommender engine as matrix
movies_matrix <- movies_clean %>% 
  column_to_rownames("movieId") %>% 
  as.matrix() %>% 
  as("binaryRatingMatrix")

# Retrieve full list of genres as a vector
genres <- movies_matrix %>% 
  colnames() %>% 
  as_tibble()

# Retrieve top 15 of movie tags to filter out rarely used tags
tags_sel <- tags %>% 
  filter(!(tag %in% c("sci-fi","action","comedy","BD-R","funny","horror",
                      "romance"))) %>% # filter out tags that are genres or irrelevant
  group_by(tag) %>% 
  tally() %>% 
  slice_max(n, n = 15)

# Retrieve top 15 of movie genres to get idea on most occurring genres or combination of genres
genre_sel <- movies %>% 
  group_by(genres) %>% 
  tally() %>% 
  slice_max(n, n = 15)

# Clean up tags, 1 per movie, separated by ",", only top 15 tags
tags_valid <- tags %>% 
  filter(tag %in% tags_sel$tag) %>% 
  group_by(movieId) %>% 
  mutate(tag = paste0(unique(tag), collapse = ",")) %>% 
  distinct()

# Add tags column
movies_full <- movies %>% 
  inner_join(ratings,
             by = "movieId") %>% 
  inner_join(tags_valid,
             by = "movieId")


# Recommender engine & predictions ----

# Set up recommender engine and perform item based collaborative filtering
recom <- movies_matrix %>% 
  Recommender(method = "IBCF", param = list(k = 5))

#recom <- movies_matrix %>% Recommender(method = "UBCF")

# Create user genre choises and preprocess as matrix
genres

genre_choise <- c("Action","","Adventure","Mystery")

genre_choise_matrix <- genres %>% 
  mutate(genre = as.numeric(value %in% genre_choise)) %>% 
  pivot_wider(names_from = value, values_from = genre) %>% 
  as.matrix() %>% 
  as("binaryRatingMatrix")

# Make predictions and retrieve highest matching genre
pred <- recom %>% 
  predict(genre_choise_matrix, n = 1)

fav_genre <- pred %>% 
  getList() %>% 
  as.character()

fav_rating <- pred %>% 
  getRatings() %>% 
  as.numeric()

# Create user tag choises
tags

tag_choise <- c("based on a book")

# Retrieve highest rated movies from fav_genre, filter for chosen tag
top5 <- movies_full %>% 
  filter(str_detect(.$genres, fav_genre) == T, str_detect(.$tag, tag_choise) == T) %>% 
  mutate(match = fav_rating * avg_rating) %>% 
  arrange(desc(match)) %>% 
  select(title, match)

