library(shiny)
library(dplyr, warn.conflicts = FALSE)

# ---------------------------- 7.1)Updating inputs ----------------------------

ui <- fluidPage(
  numericInput("min", "Minimum", 0),
  numericInput("max", "Maximum", 3),
  sliderInput("n", "n", min = 0, max = 3, value = 1)
  )
server <- function(input, output, session) {
  observeEvent(input$min, {
    updateSliderInput(session, "n", min = input$min)
    })  
  observeEvent(input$max, {
    updateSliderInput(session, "n", max = input$max)
    })
  }
shinyApp(ui,server)

# 7.1.1)Simple uses ----

ui <- fluidPage(
  sliderInput("x1", "x1", 0, min = -10, max = 10),
  sliderInput("x2", "x2", 0, min = -10, max = 10),
  sliderInput("x3", "x3", 0, min = -10, max = 10),
  actionButton("reset", "Reset")
  )
server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSliderInput(session, "x1", value = 0)
    updateSliderInput(session, "x2", value = 0)
    updateSliderInput(session, "x3", value = 0)
    })
  }
shinyApp(ui,server)

# 7.1.2)Hierarchical select boxes ----

sales <- vroom::vroom("datasets_435_896_sales_data_sample.csv", col_types = list(), na = "")
sales %>% 
  select(TERRITORY, CUSTOMERNAME, ORDERNUMBER, everything()) %>%
  arrange(ORDERNUMBER)
# Each territory contains customers.
# Each customer has multiple orders.
# Each order contains rows.

# I want to create a user interface where you can:
# Select a territory to see all customers.
# Select a customer to see all orders.
# Select an order to see the underlying rows.
ui <- fluidPage(
  selectInput("territory", "Territory", choices = unique(sales$TERRITORY)),
  selectInput("customername", "Customer", choices = NULL),
  selectInput("ordernumber", "Order number", choices = NULL),
  tableOutput("data")
  )
server <- function(input, output, session) {
  territory <- reactive({
    filter(sales, TERRITORY == input$territory)
    })
  observeEvent(territory(), {
    choices <- unique(territory()$CUSTOMERNAME)
    updateSelectInput(session, "customername", choices = choices) 
    })
  
  customer <- reactive({
    req(input$customername)
    filter(territory(), CUSTOMERNAME == input$customername)
    })
  observeEvent(customer(), {
    choices <- unique(customer()$ORDERNUMBER)
    updateSelectInput(session, "ordernumber", choices = choices)
    })
  
  output$data <- renderTable({
    req(input$ordernumber)
    customer() %>% 
      filter(ORDERNUMBER == input$ordernumber) %>% 
      select(QUANTITYORDERED, PRICEEACH, PRODUCTCODE)
    })
}
shinyApp(ui,server)

# 7.1.3)Inter-related inputs ----

ui <- fluidPage(
  numericInput("temp_c", "Celsius", NA, step = 1),
  numericInput("temp_f", "Fahrenheit", NA, step = 1)
  )
server <- function(input, output, session) {
  observeEvent(input$temp_f, {
    c <- round((input$temp_f - 32) * 5 / 9)
    updateNumericInput(session, "temp_c", value = c)
    })
  
  observeEvent(input$temp_c, {
    f <- round((input$temp_c * 9 / 5) + 32)
    updateNumericInput(session, "temp_f", value = f)
    })
  }
shinyApp(ui,server)

# --------------------------- 7.2)Dynamic visibility ---------------------------

# 7.2.1)Conditional UI ----

parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("normal",
           numericInput("mean", "mean", value = 1),
           numericInput("sd", "standard deviation", min = 0, value = 1)
           ),
  tabPanel("uniform", 
           numericInput("min", "min", value = 0),
           numericInput("max", "max", value = 1)
           ),
  tabPanel("exponential",
           numericInput("rate", "rate", value = 1, min = 0),
           )
  )

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribution", 
                  choices = c("normal", "uniform", "exponential")
                  ),
      numericInput("n", "Number of samples", value = 100),
      parameter_tabs,
      ),
    mainPanel(
      plotOutput("hist")
      )
    )
  )
server <- function(input, output, session) {
  observeEvent(input$dist, {
    updateTabsetPanel(session, "params", selected = input$dist)
    }) 
  
  sample <- reactive({
    switch(input$dist,
           normal = rnorm(input$n, input$mean, input$sd),
           uniform = runif(input$n, input$min, input$max),
           exponential = rexp(input$n, input$rate)
           )
    })
  output$hist <- renderPlot(hist(sample()), res = 96)
  }
shinyApp(ui,server)

# ------------------------- 7.3)Creating UI with code -------------------------

ui <- fluidPage(
  textInput("label", "label"),
  selectInput("type", "type", c("slider", "numeric")),
  uiOutput("numeric")
  )
server <- function(input, output, session) {
  output$numeric <- renderUI({
    if (input$type == "slider") {
      sliderInput("dynamic", input$label, value = 0, min = 0, max = 10)
      } else {
      numericInput("dynamic", input$label, value = 0, min = 0, max = 10) 
        }
    })
  }
shinyApp(ui,server)

# 7.3.1)Multiple controls ----
library(purrr)

ui <- fluidPage(
  numericInput("n", "Number of colours", value = 5, min = 1),
  uiOutput("col"),
  textOutput("palette")
  )
server <- function(input, output, session) {
  col_names <- reactive(paste0("col", seq_len(input$n)))
  
  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL))
    })
  
  output$palette <- renderText({
    map_chr(col_names(), ~ input[[.x]])
    })
  }
shinyApp(ui,server)

# 7.3.2)Dynamic filtering ----

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
    } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
    } else {
    # Not supported
    NULL
      }
  }
filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
    } else if (is.factor(x)) {
    x %in% val
      } else {
    # No control, so don't filter
    TRUE
      }
  }
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      make_ui(iris$Sepal.Length, "Sepal.Length"),
      make_ui(iris$Sepal.Width, "Sepal.Width"),
      make_ui(iris$Species, "Species")
      ),
    mainPanel(
      tableOutput("data")
      )
    )
  )
server <- function(input, output, session) {
  selected <- reactive({
    filter_var(iris$Sepal.Length, input$Sepal.Length) &
      filter_var(iris$Sepal.Width, input$Sepal.Width) &
      filter_var(iris$Species, input$Species)
    })
  
  output$data <- renderTable(head(iris[selected(), ], 12))
  }
shinyApp(ui,server)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      map(names(iris), ~ make_ui(iris[[.x]], .x))
      ),
    mainPanel(
      tableOutput("data")
      )
    )
  )
server <- function(input, output, session) {
  selected <- reactive({
    each_var <- map(names(iris), ~ filter_var(iris[[.x]], input[[.x]]))
    reduce(each_var, ~ .x & .y)
    })
  
  output$data <- renderTable(head(iris[selected(), ], 12))
  }
shinyApp(ui,server)

dfs <- keep(ls("package:datasets"), ~ is.data.frame(get(.x, "package:datasets")))
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "Dataset", choices = dfs),
      uiOutput("filter")
      ),
    mainPanel(
      tableOutput("data")
      )
    )
  )
server <- function(input, output, session) {
  data <- reactive({
    get(input$dataset, "package:datasets")
    })
  vars <- reactive(names(data()))
  
  output$filter <- renderUI(
    map(vars(), ~ make_ui(data()[[.x]], .x))
    )
  
  selected <- reactive({
    each_var <- map(vars(), ~ filter_var(data()[[.x]], input[[.x]]))
    reduce(each_var, `&`)
    })
  
  output$data <- renderTable(head(data()[selected(), ], 12))
  }
shinyApp(ui,server)
