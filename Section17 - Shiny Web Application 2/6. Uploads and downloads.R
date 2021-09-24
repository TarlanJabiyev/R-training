library(shiny)
library(rmarkdown)

# --------------------------------- 6.1)Upload ---------------------------------

# 6.1.1)UI & Server ----

ui <- fluidPage(
  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
  tableOutput("files")
  )
server <- function(input, output, session) {
  output$files <- renderTable(input$upload)
  }
shinyApp(ui, server)

# 6.1.2)Uploading data ----

ui <- fluidPage(
  fileInput("file", NULL, accept = c(".csv", ".tsv")),
  numericInput("n", "Rows", value = 5, min = 1, step = 1),
  tableOutput("head")
  )
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ","),
           tsv = vroom::vroom(input$file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
           )
    })
  
  output$head <- renderTable({
    head(data(), input$n)
    })
  }
shinyApp(ui, server)

# -------------------------------- 6.2)Download --------------------------------

# 6.2.1)Basics ----

ui <- fluidPage(
  downloadButton("download1"),
  downloadLink("download2")
  )
server <- function(input, output, session) {}
shinyApp(ui, server)

# 6.2.2)Downloading data ----

ui <- fluidPage(
  selectInput("dataset", "Pick a dataset", ls("package:datasets")),
  tableOutput("preview"),
  downloadButton("download", "Download .tsv")
  )
server <- function(input, output, session) {
  data <- reactive({
    out <- get(input$dataset, "package:datasets")
    if (!is.data.frame(out)) {
      validate(paste0("'", input$dataset, "' is not a data frame"))
      }
    out
    })
  
  output$preview <- renderTable({
    head(data())
    })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".tsv")
      },
    content = function(file) {
      vroom::vroom_write(data(), file)
      }
    )
  }
shinyApp(ui, server)

# 6.2.3)Downloading reports ----

ui <- fluidPage(
  sliderInput("n", "Number of points", 1, 100, 50),
  downloadButton("report", "Generate report")
  )
server <- function(input, output, session) {
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      params <- list(n = input$n)
      
      id <- showNotification("Rendering report...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      
      render("report.Rmd",
             output_file = file,
             params = params,
             envir = new.env(parent = globalenv())
             )
      }
    )
  }
shinyApp(ui, server)

render_report <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
                    )
  }
server <- function(input, output) {
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      params <- list(n = input$slider)
      callr::r(
        render_report,
        list(input = report_path, output = file, params = params)
        )
      }
    )
  }
shinyApp(ui, server)

# ------------------------------- 6.3)Case study -------------------------------

# #Uploading and parsing the file:
# ui_upload <- sidebarLayout(
#   sidebarPanel(
#     fileInput("file", "Data", buttonLabel = "Upload..."),
#     textInput("delim", "Delimiter (leave blank to guess)", ""),
#     numericInput("skip", "Rows to skip", 0, min = 0),
#     numericInput("rows", "Rows to preview", 10, min = 1)
#     ),
#   mainPanel(
#     h3("Raw data"),
#     tableOutput("preview1")
#     )
#   )
# 
# #Cleaning the file.
# ui_clean <- sidebarLayout(
#   sidebarPanel(
#     checkboxInput("snake", "Rename columns to snake case?"),
#     checkboxInput("constant", "Remove constant columns?"),
#     checkboxInput("empty", "Remove empty cols?")
#     ),
#   mainPanel(
#     h3("Cleaner data"),
#     tableOutput("preview2")
#     )
#   )
# 
# #Downloading the file.
# ui_download <- fluidRow(
#   column(width = 12, downloadButton("download", class = "btn-block"))
#   )
# 
# ui <- fluidPage(
#   ui_upload,
#   ui_clean,
#   ui_download
#   )
# server <- function(input, output, session) {}
# shinyApp(ui, server)
# 
# #This same organisation makes it easier to understand the app:
# server <- function(input, output, session) {
#   # Upload 
#   raw <- reactive({
#     req(input$file)
#     delim <- if (input$delim == "") NULL else input$delim
#     vroom::vroom(input$file$datapath, delim = delim, skip = input$skip)
#   })
#   output$preview1 <- renderTable(head(raw(), input$rows))
#   
#   # Clean 
#   tidied <- reactive({
#     out <- raw()
#     if (input$snake) {
#       names(out) <- janitor::make_clean_names(names(out))
#       }
#     if (input$empty) {
#       out <- janitor::remove_empty(out, "cols")
#       }
#     if (input$constant) {
#       out <- janitor::remove_constant(out)
#       }
#     
#     out
#   })
#   output$preview2 <- renderTable(head(tidied(), input$rows))
#   
#   # Download 
#   output$download <- downloadHandler(
#     filename = function() {
#       paste0(tools::file_path_sans_ext(input$file$name), ".tsv")
#       },
#     content = function(file) {
#       vroom::vroom_write(tidied(), file)
#       }
#     )
#   }
# server <- function(input, output, session) {}
# shinyApp(ui, server)
