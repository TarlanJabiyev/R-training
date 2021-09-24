# 1.1)Create app directory and file ----

library(shiny)

ui <- fluidPage(
  "Hello, world!"
  )
server <- function(input, output, session){}
shinyApp(ui, server)

# 1.2)Adding UI controls ----

ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
  )
server <- function(input, output, session){}
shinyApp(ui, server)

# 1.3)Adding behavior ----

server <- function(input, output, session) {
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    summary(dataset)
    })
  
  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
    })
  }

shinyApp(ui, server)
