# Import libraries & datasets ----
library(tidyverse)
library(hrbrthemes) 
library(plotly) 

data <- mpg

data %>% glimpse()


# GGPLOT2 ----

data %>% 
  ggplot(aes(displ, cty))

#add geometry
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point()

#add colour
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$red)

#add size 
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$red,
             aes(size = hwy))

#add labs 
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$red,
             aes(size = hwy)) +
  labs(x="engine displacement, in litres", 
       y="city miles per gallon",
       title="Fuel economy data",
       subtitle="Scatterplot")

#add scales 
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$red,
             aes(size = hwy)) +
  labs(x="engine displacement, in litres", 
       y="city miles per gallon",
       title="Fuel economy data",
       subtitle="Scatterplot") +
  scale_y_continuous(breaks = seq(0,50,2)) +
  scale_x_percent()

#add theme 
data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$yellow,
             aes(size = hwy)) +
  labs(x="engine displacement, in litres", 
       y="city miles per gallon",
       title="Fuel economy data",
       subtitle="Scatterplot") +
  theme_modern_rc()
#theme_ipsum, theme_modern_rc, theme_ft_rc

#add action :)
#add theme 
p1 <- data %>% 
  ggplot(aes(displ, cty)) +
  geom_point(color = ft_cols$yellow,
             aes(size = hwy)) +
  labs(x="engine displacement, in litres", 
       y="city miles per gallon",
       title="Fuel economy data",
       subtitle="Scatterplot") +
  theme_modern_rc()

p1 %>% ggplotly()


# Histograms

p2 <- data %>% 
  ggplot(aes(hwy)) + 
  geom_histogram(aes(fill=trans), 
                 colour="Black") +
  scale_x_continuous(breaks = seq(0,60,5)) +
  scale_y_continuous(breaks = seq(0,60,2))

p2 %>% ggplotly()


# Density plot

data$year <- data$year %>% as_factor()

data %>% 
  ggplot(aes(hwy, color = year, fill = year)) + 
  geom_density(alpha=0.4, position = 'stack') +
  scale_x_continuous(breaks = seq(10,60,5))


# Boxplot

p3 <- data %>% 
  ggplot(aes(y=hwy, colour=class)) + 
  geom_boxplot(size=1) + #coord_flip() +
  theme_ft_rc()

p3 %>% ggplotly()


# Using Facets ----

data %>% 
  mutate(total = hwy + cty) %>% 
  ggplot(aes(total)) + 
  geom_histogram(aes(fill=class), colour="Black") + 
  facet_grid(class~.) 


# Scatterplots
p <- data %>% 
  ggplot(aes(x=hwy, y=cty, colour=class))
p + geom_point(size=2)

p + geom_point(size=2) +
  facet_grid(class~., scales="free") +
  theme(legend.position="none")

p + geom_point(size=2) + 
  facet_grid(.~year, scales="free") +
  theme(legend.position="none")

p + geom_point(size=2) + 
  #geom_smooth(method = "loess") +
  facet_grid(class ~ year, scales="free") +
  theme(legend.position="none")


# Layouts ----
library(patchwork)

p1
p2
p3

p1 + p2

p1 + p2 + p3 + 
  plot_layout(nrow=2,byrow=F)

p1 / p3

p2 | (p2 / p3)

(p2 | (p2 / p3)) +
  plot_annotation('Title')

(p2 | (p2 / p3)) +
  plot_annotation(tag_levels='1')


# ggside ----
library(ggside)

data %>%
  ggplot(aes(hwy, cty, color = class)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(aes(y = after_stat(density), 
                        fill = class),
                    alpha = 0.5, size = 1,
                    position = "stack") +
  geom_ysidedensity(aes(x = after_stat(density),
                        fill = class),
                    alpha = 0.5, size = 1,
                    position = "stack") +
  labs(title = "Fuel Economy by Vehicle Type" ,
       x = "Highway MPG", 
       y = "City MPG") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4)


# Highcharter ----

library(highcharter)
#https://api.highcharts.com/highcharts/series

data %>% 
  hchart("scatter", hcaes(x = displ, y = cty)) %>%
  hc_colors(color = "red") %>%
  hc_xAxis(visible = T) %>%
  hc_yAxis(visible = T) %>% 
  hc_title(text = "Fuel economy data")

data %>% 
  group_by(model) %>% 
  summarise(hwy = max(hwy, na.rm=T)) %>% 
  hchart("pie", hcaes(x = model, y = hwy))


# ggstatsplot ----
library(dslabs)
data(gapminder)

# install.packages("remotes")
# remotes::install_github("IndrajeetPatil/ggstatsplot", dependencies=F)
# https://github.com/IndrajeetPatil/ggstatsplot/blob/master/README.md
library(ggstatsplot)

gapminder %>% 
  filter(year %in% c(2012:2015)) %>%
  ggbetweenstats(x = year,
                 y = life_expectancy,
                 plot.type = "box",
                 type = "p")

gapminder %>% 
  filter(year %in% c(2012:2015)) %>%
  ggscatterstats(x = fertility,
                 y = life_expectancy,
                 centrality.parameter = "median",
                 messages = F)

gapminder %>% 
  filter(year %in% c(2012:2015)) %>%
  ggpiestats(x = fertility,
             y = life_expectancy,
             messages = F)


# tidycharts ----

# install.packages("devtools")
# devtools::install_github("vedhav/tidycharts")
library(tidycharts)

data("tidychartsdata")
marks_data 
gender_school_earnings

gender_school_earnings %>% 
  dumbbell_chart(x1_name=Men,
                 x2_name=Women,
                 y_name=School,
                 line_color="black",
                 x1_color="blue",
                 x2_color="red",
                 show_legend=T,
                 plot_title='Gender School Earnings',
                 x_axis_title='Gender',
                 y_axis_title='School')

gender_school_earnings %>% 
  lollipop_chart(x_name=Men,
                 y_name=School,
                 line_color="green",
                 x_color="darkgreen",
                 show_legend=F,
                 plot_title='Gender School Earnings',
                 x_axis_title='Men',
                 y_axis_title='School')

marks_data %>% 
  factor_scatter_chart(x_name=marks,
                       y_name=name,
                       color_name=subject,
                       show_legend=T,
                       plot_title='Marks data',
                       x_axis_title='marks',
                       y_axis_title='name')

iris %>% 
  numeric_scatter_chart(x_name='Sepal.Length',
                        y_name='Petal.Length',
                        color_name='Species',
                        show_legend=T,
                        plot_title='Iris',
                        x_axis_title='Sepal Length',
                        y_axis_title='Petal Length')

marks_data %>% 
  group_by(name) %>% 
  summarise(marks = mean(marks)) %>% 
  bar_chart(x_name=name,
            y_name=marks,
            static_color="blue",
            border_line_color="green",
            border_line_width=2,
            stack=F,
            highlight=c('Danny','Jon'),
            show_legend=F,
            sort_colors_alphabetically=F,
            plot_title="Grouped Marks Data")

marks_data %>% 
  bar_chart(x_name=marks,
            y_name=name,
            color_name=subject,
            border_line_width=2,
            stack=F,
            show_legend=T,
            sort_colors_alphabetically=T,
            plot_title="Marks Data")

marks_data %>% 
  bar_chart(x_name=name,
            y_name=marks,
            color_name=subject,
            border_line_width=2,
            border_line_color='bold',
            stack=T,
            show_legend=T,
            sort_colors_alphabetically=T,
            plot_title="Marks Data")
