# Import libraries & dataset ----
library(tidyverse)
library(plotly)
library(factoextra) 
library(NbClust) 
library(cluster) 

data("iris")

iris %>% glimpse()

iris$Species %>% table()

# Standardize the data
df <- iris %>% select(-Species) %>% scale()


# Optimal Number of Clusters ----

df %>% 
  fviz_nbclust(kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
# Elbow method: 2 clusters solution suggested

df %>% 
  fviz_nbclust(kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
# Silhouette method: 2 clusters solution suggested

df %>% 
  fviz_nbclust(kmeans, method = "gap_stat")+
  labs(subtitle = "Gap statistic method")
# Gap statistic method: 2 clusters solution suggested


# Fitting K-Means to the data ----
set.seed(123)
kmeans <- df %>% kmeans(centers = 2)
y_kmeans <- kmeans$cluster %>% as_factor()

df %>% clusplot(y_kmeans,
                shade = TRUE,
                color = TRUE,
                labels = 2,
                plotchar = F,
                main = 'Clusters of customers')


# Visualize the result ----

g <- iris %>% 
  ggplot(aes(Sepal.Length,Petal.Length,
             color = y_kmeans)) +
  geom_point(aes(text = Species),size = 2) + 
  #facet_wrap(~ Species) +
  scale_x_continuous(breaks = seq(0,150,10)) +
  scale_y_continuous(breaks = seq(0,150,10)) +
  labs(x="Sepal Length", 
       y="Petal Length",
       title="Iris",
       subtitle="4 clusters")

g %>% ggplotly(tooltip = "text")
