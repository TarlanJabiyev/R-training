# Import libraries & dataset ----
library(tidyverse) 
library(cowplot)

hr_people_analytics_tbl <- read_csv("HR-Employee-Attrition.csv")


# Create a plot ----
hr_people_analytics_char_tbl <- hr_people_analytics_tbl %>% 
  select_if(is.character)

hr_people_analytics_char_tbl %>% 
  count(Attrition) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = fct_reorder(Attrition, prop),
             y = prop,
             color = Attrition)) +
  geom_point(aes(size = prop),
             show.legend = F) +
  geom_segment(aes(xend = Attrition, yend = 0),
               show.legend = F) +
  geom_label(aes(label = prop, size = prop*10), 
             fill = "white", 
             hjust = "inward",
             show.legend = F) +
  labs(x = "Attrition") +
  coord_flip() +
  theme_minimal()


# Creating our plotting function ----
names <- hr_people_analytics_char_tbl %>% names()
names <- names %>% set_names()

plot_frequency <- function(x) {
  
  hr_people_analytics_char_tbl %>% 
    count(.data[[x]]) %>% 
    mutate(prop = n/sum(n)) %>% 
    ggplot(aes(x = fct_reorder(.data[[x]], prop),
               y = prop,
               color = .data[[x]])) +
    geom_point(aes(size = prop),
               show.legend = F) +
    geom_segment(aes(xend = .data[[x]], yend = 0),
                 show.legend = F) +
    geom_label(aes(label = prop, size = prop*10), 
               fill = "white", 
               hjust = "inward",
               show.legend = F) +
    labs(x = x) +
    coord_flip() +
    theme_minimal()
}

all_plots <- names %>% map(plot_frequency)

plot_grid(plotlist = all_plots)
