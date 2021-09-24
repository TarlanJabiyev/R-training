library(shiny)
library(ggplot2)
#remotes::install_github("hadley/dplyr")
library(dplyr, warn.conflicts = FALSE)

# ------------------------------ 8.1)Data-masking ------------------------------

# 8.1.1)Indirection ----

min_carat <- 1
diamonds %>% filter(carat > min_carat)

var <- "carat"
min <- 1
diamonds[diamonds[[var]] > min, ]

diamonds %>% filter(.data$carat > min)

var <- "carat"
diamonds %>% filter(.data[[var]] > min)

ui <- fluidPage(
  selectInput("var", "Variable", choices = names(diamonds)),
  numericInput("min", "Minimum", value = 1),
  tableOutput("output")
  )
server <- function(input, output, session) {
  data <- reactive(filter(diamonds, .data[[input$var]] > input$min))
  output$output <- renderTable(head(data()))
  }
shinyApp(ui,server)

server <- function(input, output, session) {
  data <- reactive(filter(diamonds, input$var > input$min))
  output$output <- renderTable(head(data()))
}
shinyApp(ui,server)

# 8.1.2)Example: ggplot2 ----

ui <- fluidPage(
  selectInput("x", "X variable", choices = names(iris)),
  selectInput("y", "Y variable", choices = names(iris)),
  plotOutput("plot")
  )
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(iris, aes(.data[[input$x]], .data[[input$y]])) +
      geom_point(position = ggforce::position_auto())
    }, res = 96)
  }
shinyApp(ui,server)

ui <- fluidPage(
  selectInput("x", "X variable", choices = names(iris)),
  selectInput("y", "Y variable", choices = names(iris)),
  selectInput("geom", "geom", c("point", "smooth", "jitter")),
  plotOutput("plot")
  )
server <- function(input, output, session) {
  plot_geom <- reactive({
    switch(input$geom,
           point = geom_point(),
           smooth = geom_smooth(se = FALSE),
           jitter = geom_jitter()
           )
    })
  
  output$plot <- renderPlot({
    ggplot(iris, aes(.data[[input$x]], .data[[input$y]])) +
      plot_geom()
    }, res = 96)
  }
shinyApp(ui,server)

# 8.1.3)Example: dplyr ----

ui <- fluidPage(
  selectInput("var", "Select variable", choices = names(mtcars)),
  sliderInput("min", "Minimum value", 0, min = 0, max = 100),
  selectInput("sort", "Sort by", choices = names(mtcars)),
  tableOutput("data")
  )
server <- function(input, output, session) {
  observeEvent(input$var, {
    rng <- range(mtcars[[input$var]])
    updateSliderInput(session, "min", value = rng[[1]], min = rng[[1]], max = rng[[2]])
    })
  
  output$data <- renderTable({
    mtcars %>% 
      filter(.data[[input$var]] > input$min) %>% 
      arrange(.data[[input$sort]])
    })
}
shinyApp(ui,server)

ui <- fluidPage(
  selectInput("var", "Sort by", choices = names(mtcars)),
  checkboxInput("desc", "Descending order?"),
  tableOutput("data")
  )
server <- function(input, output, session) {
  sorted <- reactive({
    if (input$desc) {
      arrange(mtcars, desc(.data[[input$var]]))
      } else {
      arrange(mtcars, .data[[input$var]])
        }
    })
  output$data <- renderTable(sorted())
  }
shinyApp(ui,server)

# 8.1.4)User supplied data ----

ui <- fluidPage(
  fileInput("data", "dataset", accept = ".tsv"),
  selectInput("var", "var", character()),
  numericInput("min", "min", 1, min = 0, step = 1),
  tableOutput("output")
  )
server <- function(input, output, session) {
  data <- reactive({
    req(input$data)
    vroom::vroom(input$data$datapath)
    })
  observeEvent(data(), {
    updateSelectInput(session, "var", choices = names(data()))
    })
  observeEvent(input$var, {
    val <- data()[[input$var]]
    updateNumericInput(session, "min", value = min(val))
    })
  
  output$output <- renderTable({
    req(input$var)
    
    data() %>% 
      filter(.data[[input$var]] > input$min) %>% 
      arrange(.data[[input$var]]) %>% 
      head(10)
    })
  }
shinyApp(ui,server)

# ----------------------------- 8.2)Tidy-selection -----------------------------

ui <- fluidPage(
  selectInput("vars", "Variables", names(mtcars), multiple = TRUE),
  tableOutput("data")
  )
server <- function(input, output, session) {
  output$data <- renderTable({
    req(input$vars)
    mtcars %>% select(all_of(input$vars))
    })
  }
shinyApp(ui,server)