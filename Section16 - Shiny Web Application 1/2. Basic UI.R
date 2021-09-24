library(shiny)

# --------------------------------- 2.1)Inputs ---------------------------------

# 2.1.1)Free text ----

ui <- fluidPage(
  textInput("name", "What's your name?"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3)
  )
server <- function(input, output, session){}
shinyApp(ui, server)

# 2.1.2)Numeric inputs ----

ui <- fluidPage(
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
  sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100)
  )
shinyApp(ui, server)

# 2.1.3)Dates ----

ui <- fluidPage(
  dateInput("dob", "When were you born?"),
  dateRangeInput("holiday", "When do you want to go on vacation next?")
  )
shinyApp(ui, server)

# 2.1.4)Limited choices ----

animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")

ui <- fluidPage(
  selectInput("state", "What's your favourite state?", state.name),
  radioButtons("animal", "What's your favourite animal?", animals)
  )
shinyApp(ui, server)

ui <- fluidPage(
  radioButtons("rb", "Choose one:",
               choiceNames = list(
                 icon("angry"),
                 icon("smile"),
                 icon("sad-tear")
               ),
               choiceValues = list("angry", "happy", "sad")
               )
  )
shinyApp(ui, server)

ui <- fluidPage(
  selectInput(
    "state", "What's your favourite state?", state.name,
    multiple = T
    )
  )
shinyApp(ui, server)

ui <- fluidPage(
  checkboxGroupInput("animal", "What animals do you like?", animals)
  )
shinyApp(ui, server)

ui <- fluidPage(
  checkboxInput("cleanup", "Clean up?", value = TRUE),
  checkboxInput("shutdown", "Shutdown?")
  )
shinyApp(ui, server)

# 2.1.5)File uploads ----

ui <- fluidPage(
  fileInput("upload", NULL)
  )
shinyApp(ui, server)

# 2.1.6)Action buttons ----

ui <- fluidPage(
  actionButton("click", "Click me!"),
  actionButton("drink", "Drink me!", icon = icon("cocktail"))
  )
shinyApp(ui, server)
# http://bootstrapdocs.com/v3.3.6/docs/css/#buttons

ui <- fluidPage(
  fluidRow(
    actionButton("click", "Click me!", class = "btn-danger"),
    actionButton("drink", "Drink me!", class = "btn-lg btn-success")
    ),
  fluidRow(
    actionButton("eat", "Eat me!", class = "btn-block")
    )
  )
shinyApp(ui, server)

# -------------------------------- 2.2)Outputs --------------------------------

# 2.2.1)Text ----

ui <- fluidPage(
  textOutput("text"),
  verbatimTextOutput("code")
  )
server <- function(input, output, session) {
  output$text <- renderText({ 
    "Hello friend!" 
    })
  output$code <- renderPrint({ 
    summary(1:10) 
    })
  }
shinyApp(ui, server)

server <- function(input, output, session) {
  output$text <- renderText("Hello friend!")
  output$code <- renderPrint(summary(1:10))
}
shinyApp(ui, server)

# 2.2.2)Tables ----

ui <- fluidPage(
  tableOutput("static"),
  dataTableOutput("dynamic")
  )
server <- function(input, output, session) {
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
}
shinyApp(ui, server)

# 2.2.3)Plots ----

ui <- fluidPage(
  plotOutput("plot", width = "400px")
  )
server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5), res = 96)
  }
shinyApp(ui, server)

# -------------------------------- 2.3)Layouts --------------------------------

# 2.3.1)Page with sidebar ----

# page

ui <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel(
      numericInput("m", "Number of samples:", 2, min = 1, max = 100)
      ),
    mainPanel(
      plotOutput("hist")
      )
    )
  )

server <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
    }, res = 96)
  }

shinyApp(ui, server)

# 2.3.2)Multi-row ----

# page

# 2.3.3)Themes ----

# https://rstudio.github.io/shinythemes/

theme_demo <- function(theme) {
  fluidPage(
    theme = shinythemes::shinytheme(theme),
    sidebarLayout(
      sidebarPanel(
        textInput("txt", "Text input:", "text here"),
        sliderInput("slider", "Slider input:", 1, 100, 30)
        ),
      mainPanel(
        h1("Header 1"),
        h2("Header 2")
        )
      )
    )
  }
theme_demo("darkly")

ui <- fluidPage(
  theme_demo("darkly")
)
server <- function(input, output, session){}
shinyApp(ui, server)