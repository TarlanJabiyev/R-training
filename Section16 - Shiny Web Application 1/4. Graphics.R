library(shiny)
library(ggplot2)

# ----------------------------- 4.1)Interactivity -----------------------------

# 4.1.1)Basics ----

ui <- basicPage(
  plotOutput("plot", click = "plot_click"),
  verbatimTextOutput("info")
  )

server <- function(input, output) {
  output$plot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
    }, res = 96)
  
  output$info <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
    })
}

shinyApp(ui, server)

# 4.1.2)Clicking ----

ui <- fluidPage(
  plotOutput("plot", click = "click"),
  tableOutput("data")
  )
server <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
    }, res = 96)
  
  output$data <- renderTable({
    nearPoints(mtcars, input$click, xvar = "wt", yvar = "mpg")
    })
  }
shinyApp(ui, server)

ui <- fluidPage(
  plotOutput("plot", click = "plot_click"),
  tableOutput("data")
  )
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) + geom_point()
    }, res = 96)
  
  output$data <- renderTable({
    nearPoints(mtcars, input$plot_click)
    })
  }
shinyApp(ui, server)

# 4.1.3)Brushing ----

ui <- fluidPage(
  plotOutput("plot", brush = "plot_brush"),
  tableOutput("data")
  )
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) + geom_point()
    }, res = 96)
  
  output$data <- renderTable({
    brushedPoints(mtcars, input$plot_brush)
    })
  }
shinyApp(ui, server)

# ------------------------ 4.2)Dynamic height and width ------------------------

ui <- fluidPage(
  sliderInput("height", "height", min = 100, max = 500, value = 250),
  sliderInput("width", "width", min = 100, max = 500, value = 250),
  sliderInput("n", "n", min = 10, max = 100, value = 25),
  plotOutput("plot", width = 250, height = 250)
  )
server <- function(input, output, session) {
  output$plot <- renderPlot(
    width = function() input$width,
    height = function() input$height,
    res = 96,
    {
      plot(rnorm(input$n), rnorm(input$n))
      }
    )
  }
shinyApp(ui, server)
