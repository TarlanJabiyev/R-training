library(shiny)
library(purrr)

# --------------------------------- Motivation ---------------------------------

# https://www.youtube.com/watch?v=ylLLVo2VL50&feature=youtu.be

# --------------------------------- Case study ---------------------------------

# 1.Wizard ----

nextPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i + 1)), "next")
  }
prevPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i - 1)), "prev")
  }

wrapPage <- function(title, page, button_left = NULL, button_right = NULL) {
  tabPanel(
    title = title, 
    fluidRow(
      column(12, page)
      ), 
    fluidRow(
      column(6, button_left),
      column(6, button_right)
      )
    )
  }

wizardUI <- function(id, pages, doneButton = NULL) {
  stopifnot(is.list(pages))
  n <- length(pages)
  
  wrapped <- vector("list", n)
  for (i in seq_along(pages)) {
    # First page only has next; last page only prev + done
    lhs <- if (i > 1) prevPage(id, i)
    rhs <- if (i < n) nextPage(id, i) else doneButton
    wrapped[[i]] <- wrapPage(paste0("page_", i), pages[[i]], lhs, rhs)
    }
  
  # Create tabsetPanel
  # https://github.com/rstudio/shiny/issues/2927
  wrapped$id <- NS(id, "wizard")
  wrapped$type <- "hidden"
  do.call("tabsetPanel", wrapped)
  }

wizardServer <- function(id, n) {
  moduleServer(id, function(input, output, session) {
    changePage <- function(from, to) {
      observeEvent(input[[paste0("go_", from, "_", to)]], {
        updateTabsetPanel(session, "wizard", selected = paste0("page_", to))
        })  
      }
    ids <- seq_len(n)
    lapply(ids[-1], function(i) changePage(i, i - 1))
    lapply(ids[-n], function(i) changePage(i, i + 1))
    })
  }

wizardApp <- function(...) {
  pages <- list(...)
  
  ui <- fluidPage(
    wizardUI("whiz", pages)
    )
  server <- function(input, output, session) {
    wizardServer("whiz", length(pages))
    }
  shinyApp(ui, server)
  }

page1 <- tagList(
  textInput("name", "What's your name?")
  )
page2 <- tagList(
  numericInput("age", "How old are you?", 20)
  )
page3 <- tagList(
  "Is this data correct?",
  verbatimTextOutput("info")
  )

ui <- fluidPage(
  wizardUI(
    id = "demographics", 
    pages = list(page1, page2, page3), 
    doneButton = actionButton("done", "Submit")
    )
  )
server <- function(input, output, session) {
  wizardServer("demographics", 3)
  
  observeEvent(input$done, showModal(
    modalDialog("Thank you!", footer = NULL)
    ))
  
  output$info <- renderText(paste0(
    "Age: ", input$age, "\n",
    "Name: ", input$name, "\n"
    ))
  }
shinyApp(ui,server)

# --------------------------- Single object modules ---------------------------

histogramUI <- function(id, df) {
  tagList(
    selectInput(NS(id, "var"), "Variable", names(df)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
    )
  }

histogramServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(df[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
      }, res = 96)
    })
  }

ui <- fluidPage(
  tabsetPanel(
    tabPanel("mtcars", histogramUI("mtcars", mtcars)),
    tabPanel("iris", histogramUI("iris", iris))
    )
  )
server <- function(input, output, session) {
  histogramServer("mtcars", mtcars)
  histogramServer("iris", iris)
  }
shinyApp(ui,server)

