# Exploratory Data Analyzing 

library(tidyverse)
library(data.table)

dataset <- fread('african_names.csv')

dataset %>% glimpse()


library(skimr)
dataset %>% skim()


library(inspectdf)
dataset %>% inspect_na() %>% show_plot()
dataset %>% inspect_cor() %>% show_plot()
dataset %>% inspect_num() %>% show_plot()
dataset %>% inspect_cat() %>% show_plot()


library(naniar)
dataset %>%
  select(gender, age, height, year_arrival) %>%
  gg_miss_upset()


dataset %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric) %>% 
  gather() %>% 
  ggplot(aes(x = value, group = key)) +
  geom_histogram(fill = "green", color = "darkgreen",bins = 30) +
  facet_wrap(~ key, ncol = 4, scale = "free")


library(highcharter)
dataset %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, as.numeric) %>%
  na.omit() %>% 
  cor() %>% 
  round(2) %>% 
  hchart(label = T)


library(corrmorant)
iris %>% 
  ggcorrm() +
  lotri(geom_point(alpha = 0.5)) +
  lotri(geom_smooth()) +
  utri_heatmap() +
  utri_corrtext() +
  dia_names(y_pos = 0.15, size = 3) +
  dia_histogram(lower = 0.3, fill = "grey80", color = 1) +
  scale_fill_corr() +
  labs(title = "Correlation Plot")


library(correlationfunnel)
correlation <- iris %>% binarize(one_hot = T) 

correlation %>% 
  correlate(target = Species__virginica) %>% 
  plot_correlation_funnel(interactive = F)


library(explore)

iris %>% describe()

iris %>% describe(Species)
iris %>% describe(Sepal.Length)

iris %>% explore()

iris %>% explore_tbl()

iris %>% describe_tbl()

iris %>% explore(Species)
iris %>% explore(Sepal.Length)

iris %>% explore(Sepal.Length, target = Species)
iris %>% explore(Sepal.Length, Petal.Length, target = Species)

iris %>% report(output_file = "report.html", output_dir = tempdir())

# Report of all variables and their relationship with a binary target
iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)

iris %>% explore(Sepal.Length, target = is_versicolor)
iris %>% explore(Sepal.Length, target = is_versicolor, split = FALSE)

iris %>% 
  report(output_file = "report.html", 
         output_dir = tempdir(),
         target = is_versicolor)

iris %>% 
  select(Sepal.Length, Sepal.Width) %>% 
  explore_all()
iris %>% 
  select(Sepal.Length, Sepal.Width, is_versicolor) %>% 
  explore_all(target = is_versicolor)
iris %>% 
  select(Sepal.Length, Sepal.Width, is_versicolor) %>% 
  explore_all(target = is_versicolor, split = FALSE)
iris %>% 
  select(Sepal.Length, Sepal.Width, Species) %>% 
  explore_all(target = Species)


library(dlookr)
dataset %>% 
  diagnose_report(output_format = "html", browse = T)


library(esquisse)
dataset %>% esquisser()


library(easyalluvial)
library(parcats)

mtcars2 %>% 
  select(cyl,vs,am) %>% 
  alluvial_wide() %>% 
  parcats(marginal_histograms = T, 
          data_input = mtcars2)
