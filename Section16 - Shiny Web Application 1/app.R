library(tidyverse)
library(data.table)
library(inspectdf)
library(shiny)

# The data ----
injuries <- fread("injuries.csv")
products <- fread("products.csv")
population <- fread("population.csv")

# EDA ----
injuries %>% inspect_na()
products %>% inspect_na()
population %>% inspect_na()

# Look at the product associated with the most injuries
injuries$prod_code %>% 
  as.factor() %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  .[1,1] -> pc

products$title[products$prod_code == pc]

stair_step <- injuries %>% filter(prod_code == 1842)

# Some basic summaries looking at the diagnosis, body part, and location where the injury occurred.
stair_step %>% count(diag, sort = T)

stair_step %>% count(body_part, sort = T)

stair_step %>% count(location, sort = T)


stair_step %>% 
  count(age, sex) %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")
# We see a big spike when children are learning to walk, a flattening off over middle age, and then 
# a gradual decline after age 50. Interestingly, the number of injuries is much higher for women.

# One problem with interpreting this pattern is that we know that there are fewer 
# older people than younger people, so the population available to be injured is 
# smaller. We can control for this by comparing the number of people injured with 
# the total population and calculating an injury rate. Here I use a rate per 10,000.
stair_step %>% 
  count(age, sex) %>% 
  merge(population, by = c("age", "sex"), all.x = T) %>% 
  mutate(rate = n / population * 10000) %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line() + 
  labs(y = "Injuries per 10,000 people")

# App ----

ui <- fluidPage(
  fluidRow(
    column(10, selectInput("code", "Product", products$title))
    ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
    ),
  fluidRow(
    column(12, plotOutput("age_sex"))
    )
  )

server <- function(input, output, session) {
  data <- reactive(injuries %>% 
                         merge(products, by = "prod_code", all.x = T) %>% 
                         filter(title == input$code))
  
  output$diag <- renderTable(
    data() %>% count(diag, sort = T)
    )
  output$body_part <- renderTable(
    data() %>% count(body_part, sort = T)
    )
  output$location <- renderTable(
    data() %>% count(location, sort = T)
    )
  
  output$age_sex <- renderPlot({
    data() %>%
      count(age, sex) %>%
      merge(population, by = c("age", "sex"), all.x = T) %>%
      mutate(rate = n / population * 10000) %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Injuries per 10,000 people")
    }, res = 96)
  }

shinyApp(ui, server)
