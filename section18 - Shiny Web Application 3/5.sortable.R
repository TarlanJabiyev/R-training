library(tidyverse)
library(shiny)
library(htmltools)
library(sortable)
library(DiagrammeR)


# Rank list ----

labels <- list(
  "one", "two", "three",
  tags$div(em("Complex"), " html tag without a name"),
  "five" = tags$div(em("Complex"), " html tag with name: 'five'")
  )

rank_list_basic <- rank_list(
  text = "Drag the items in any desired order",
  labels = labels,
  input_id = "rank_list_basic"
  )

rank_list_swap <- rank_list(
  text = "Notice that dragging causes items to swap",
  labels = labels,
  input_id = "rank_list_swap",
  options = sortable_options(swap = T)
  )

rank_list_multi <- rank_list(
  text = "You can select multiple items, then drag as a group",
  labels = labels,
  input_id = "rank_list_multi",
  options = sortable_options(multiDrag = T)
  )


ui <- fluidPage(
  fluidRow(
    column(width = 12,
           tags$h2("Default, multi-drag and swapping behaviour"),
           tabsetPanel(type = "tabs",
                       tabPanel("Default",
                                tags$b("Exercise"),
                                rank_list_basic,
                                tags$b("Result"),
                                verbatimTextOutput("results_basic")),
                       tabPanel("Multi-drag",
                                tags$b("Exercise"),
                                rank_list_multi,
                                tags$b("Result"),
                                verbatimTextOutput("results_multi")),
                       tabPanel("Swap",
                                tags$b("Exercise"),
                                rank_list_swap,
                                tags$b("Result"),
                                verbatimTextOutput("results_swap"))
                       )
           )))

server <- function(input, output) {
  output$results_basic <- renderPrint({
    input$rank_list_basic # This matches the input_id of the rank list
  })
  output$results_multi <- renderPrint({
    input$rank_list_multi # This matches the input_id of the rank list
  })
  output$results_swap <- renderPrint({
    input$rank_list_swap # This matches the input_id of the rank list
  })
}

shinyApp(ui, server)


# Bucket list ----

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
  fluidRow(
    column(tags$b("Exercise"), width = 12,
           bucket_list(header = "Drag the items in any desired bucket",
                       group_name = "bucket_list_group",
                       orientation = "horizontal",
                       add_rank_list(text = "Drag from here",
                                     labels = list("one", "two", "three",
            tags$div(
              htmltools::em("Complex"), " html tag without a name"
            ),
            "five" = htmltools::tags$div(
              htmltools::em("Complex"), " html tag with name: 'five'"
            )
          ),
          input_id = "rank_list_1"
        ),
        add_rank_list(
          text = "to here",
          labels = NULL,
          input_id = "rank_list_2"
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$b("Result"),
      column(
        width = 12,
        
        tags$p("input$rank_list_1"),
        verbatimTextOutput("results_1"),
        
        tags$p("input$rank_list_2"),
        verbatimTextOutput("results_2"),
        
        tags$p("input$bucket_list_group"),
        verbatimTextOutput("results_3")
      )
    )
  )
)

server <- function(input,output) {
  output$results_1 <-
    renderPrint(
      input$rank_list_1 # This matches the input_id of the first rank list
    )
  output$results_2 <-
    renderPrint(
      input$rank_list_2 # This matches the input_id of the second rank list
    )
  output$results_3 <-
    renderPrint(
      input$bucket_list_group # Matches the group_name of the bucket list
    )
}


shinyApp(ui, server)


# Add drag-and-drop to any HTML element ----

html_print(tagList(
  tags$p("You can drag and drop the diagrams to switch order:"),
  tags$div(id = "aUniqueId",
           tags$div(style = "border: solid 0.2em gray; float:left; margin: 5px",
                    mermaid("graph LR; S[SortableJS] -->|sortable| R ",
                            height = 250, width = 300)),
           tags$div(style = "border: solid 0.2em gray; float:left; margin: 5px",
                    mermaid("graph TD; JavaScript -->|htmlwidgets| R ",
                            height = 250, width = 150))
           ),
  sortable_js("aUniqueId") # the CSS id
  ))
