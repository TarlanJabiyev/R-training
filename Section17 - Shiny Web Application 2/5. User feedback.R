library(shiny)
#remotes::install_github("merlinoa/shinyFeedback", build_vignettes = T)
library(shinyFeedback)

# ------------------------------- 5.1)Validation -------------------------------

# 5.1.1)Validating input ----

ui <- fluidPage(
  useShinyFeedback(),
  numericInput("n", "n", value = 10),
  textOutput("half")
  )
server <- function(input, output, session) {
  observeEvent(input$n,
               shinyFeedback::feedbackWarning(
                 "n", 
                 input$n %% 2 != 0,
                 "Please select an even number"
                 )
               )
  output$half <- renderText(input$n / 2)
}
shinyApp(ui, server)

server <- function(input, output, session) {
  half <- reactive({
    even <- input$n %% 2 == 0
    shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
    req(even)
    input$n / 2    
    })
  
  output$half <- renderText(half())
  }
shinyApp(ui, server)

# 5.1.2)Pausing execution with req() ----

ui <- fluidPage(
  selectInput("language", "Language", choices = c("", "English", "Maori")),
  textInput("name", "Name"),
  textOutput("greeting")
  )
server <- function(input, output, session) {
  greetings <- c(
    English = "Hello", 
    Maori = "Ki ora"
    )
  output$greeting <- renderText({
    paste0(greetings[[input$language]], " ", input$name, "!")
    })
}
shinyApp(ui, server)

server <- function(input, output, session) {
  greetings <- c(
    English = "Hello", 
    Maori = "Ki ora"
    )
  output$greeting <- renderText({
    req(input$language, input$name)
    paste0(greetings[[input$language]], " ", input$name, "!")
    })
  }
shinyApp(ui, server)

# 5.1.3)req() and validation ----

ui <- fluidPage(
  useShinyFeedback(),
  textInput("dataset", "Dataset name"), 
  tableOutput("data")
  )
server <- function(input, output, session) {
  data <- reactive({
    req(input$dataset)
    
    exists <- exists(input$dataset, "package:datasets")
    shinyFeedback::feedbackDanger("dataset", !exists, "Unknown dataset")
    req(exists, cancelOutput = TRUE)
    
    get(input$dataset, "package:datasets")
    })
  
  output$data <- renderTable({
    head(data())
    })
}
shinyApp(ui, server)

# 5.1.4)Validate output ----

ui <- fluidPage(
  numericInput("x", "x", value = 0),
  selectInput("trans", "transformation", choices = c("square", "log", "square-root")),
  textOutput("out")
  )
server <- function(input, output, server) {
  output$out <- renderText({
    if (input$x < 0 && input$trans %in% c("log", "square-root")) {
      validate("x can not be negative for this transformation")
      }
    
    switch(input$trans,
           square = input$x ^ 2,
           "square-root" = sqrt(input$x),
           log = log(input$x)
           )
    })
  }
shinyApp(ui, server)

# ----------------------------- 5.2)Notifications -----------------------------

ui <- fluidPage(
  actionButton("goodnight", "Good night")
  )
server <- function(input, output, session) {
  observeEvent(input$goodnight, {
    showNotification("So long")
    Sys.sleep(1)
    showNotification("Farewell")
    Sys.sleep(1)
    showNotification("Auf Wiedersehen")
    Sys.sleep(1)
    showNotification("Adieu")
    })
  }
shinyApp(ui, server)

server <- function(input, output, session) {
  observeEvent(input$goodnight, {
    showNotification("So long")
    Sys.sleep(1)
    showNotification("Farewell", type = "message")
    Sys.sleep(1)
    showNotification("Auf Wiedersehen", type = "warning")
    Sys.sleep(1)
    showNotification("Adieu", type = "error")
    })
  }
shinyApp(ui, server)

# ------------------------------- 5.3)Progress bars -------------------------------

ui <- fluidPage(
  numericInput("steps", "How many steps?", 10),
  actionButton("go", "go"),
  textOutput("result")
  )
server <- function(input, output, session) {
  data <- reactive({
    req(input$go)
    
    progress <- Progress$new(max = input$steps)
    on.exit(progress$close())
    
    progress$set(message = "Computing random number")
    for (i in seq_len(input$steps)) {
      Sys.sleep(0.5)
      progress$inc(1)
      }
    runif(1)
    })
  
  output$result <- renderText(round(data(), 2))
  }
shinyApp(ui, server)

# --------------------------- 5.4)Confirming and undoing ---------------------------

modal_confirm <- modalDialog(
  "Are you sure you want to continue?",
  title = "Deleting files",
  footer = tagList(
    actionButton("cancel", "Cancel"),
    actionButton("ok", "Delete", class = "btn btn-danger")
    )
  )

ui <- fluidPage(
  actionButton("delete", "Delete all files?")
  )
server <- function(input, output, session) {
  observeEvent(input$delete, {
    showModal(modal_confirm)
    })
  
  observeEvent(input$ok, {
    showNotification("Files deleted")
    removeModal()
    })
  observeEvent(input$cancel, 
               removeModal()
               )
}
shinyApp(ui, server)
