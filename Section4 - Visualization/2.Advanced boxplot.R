library(tidyverse)
library(tidyquant)


library(gghalves)

# Basic plot
mpg %>%
    filter(cyl != 5) %>%
    mutate(cyl = factor(cyl)) %>%
    ggplot(aes(cyl, hwy)) +
    geom_jitter() +
    theme_tq()

# Boxplot: Distribution
mpg %>%
    filter(cyl != 5) %>%
    mutate(cyl = factor(cyl)) %>%
    ggplot(aes(cyl, hwy)) +
    geom_boxplot(outlier.colour = "red") +
    theme_tq()

# 3.0 Half-Boxplot / Half-Dotplot
mpg %>%
    filter(cyl != 5) %>%
    mutate(cyl = factor(cyl)) %>%
    ggplot(aes(cyl, hwy, color = cyl)) +
    geom_half_boxplot(outlier.color = "red") +
    geom_half_dotplot(aes(fill = cyl),
                      dotsize = 0.75,
                      stackratio = 0.5,
                      color = "black") +
    labs(title = "Highway Fuel Economy by Engine Size",
         subtitle = "Half-Boxplot + Half-Dotplot") +
    facet_grid(cols = vars(cyl), scales = "free_x") +
    scale_color_tq() +
    scale_fill_tq() +
    theme_tq() 

# Inspect cyl 6
mpg %>%
    filter(cyl == 6) %>%
    ggplot(aes(class, hwy, fill = class)) +
    geom_boxplot() +
    labs(title = "6 Cylinder Vehicles: Pickup and SUV causing Bi-Modal Relationship") +
    scale_fill_tq() +
    theme_tq()

mpg %>%
    filter(cyl == 6) %>%
    ggplot(aes(class, hwy, fill = class)) +
    geom_half_boxplot(
        outlier.colour = "red") +
    geom_half_dotplot(aes(fill = class),
                      dotsize = 0.75,
                      stackratio = 0.5,
                      color = "black") +
    labs(title = "6 Cylinder Vehicles: Pickup and SUV causing Bi-Modal Relationship") +
    scale_color_tq() +
    scale_fill_tq() +
    theme_tq() 


library(ggdist)

mpg %>%
    filter(cyl %in% c(4,6,8)) %>%
    ggplot(aes(x = factor(cyl), y = hwy, fill = factor(cyl))) +
    stat_halfeye(
        adjust = 0.5, #custom bandwidth
        justification = -.2, #move geom to the right
        .width = 0, #remove slab interval
        point_colour = NA) +
    geom_boxplot(
        width = .12,
        outlier.color = NA, #remove outliers
        alpha = 0.5) +
    stat_dots(
        side = "left", #orientation to the left
        justification = 1.1, #move geom to the left
        binwidth = .25) + #adjust grouping (binning) of observations
    labs(title = "Raincloud Plot",
         subtitle = "Showing the Bi-Modal Distribution of 6 Cylinder Vehicles",
         x = "Engine Size (No. of Cylinders)",
         y = "Highway Fuel Economy (MPG)",
         fill = "Cylinders") +
    coord_flip() +
    scale_fill_tq() +
    theme_tq() 
