# Import libraries & datasets ----
library(tidyverse)
library(data.table)
library(dslabs)
library(shiny)
library(bslib)
library(plotly)
library(inspectdf)
library(DataExplorer)
library(highcharter)
library(corrmorant)
library(skimr)
library(naniar)

riskfactors <- riskfactors
gapminder <- gapminder
pokemon <- pokemon
starwars <- starwars
storms <- storms

data_list <- list("Risk Factors" = riskfactors,
                  "Gapminder" = gapminder,
                  "Storms" = storms,
                  "Starwars" = starwars,
                  "Pokemon" = pokemon)


# Shinyapp ----

ui <- navbarPage(
  title = "Exploratory Data Analyzing",
  theme = bs_theme(version = 4, bootswatch = "minty"),
  tabPanel(
    title = "required packages",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h1("Explore a Dataset"),
        selectInput(
          inputId = "dataset_choice",
          label   = "Data Connection",
          choices = c("Risk Factors","Gapminder","Storms",
                      "Starwars","Pokemon"))),
      mainPanel(
        h3("Histogram"),
        h6("ggplot()"),
        plotOutput("Histogram", height = 800),
        
        h3("inspectdf"),
        h6("show_plot(inspect_num())"),
        plotOutput("inspectdf1", height = 600),
        
        h3("inspectdf"),
        h6("show_plot(inspect_cat())"),
        plotOutput("inspectdf2", height = 600),
        
        h3("inspectdf"),
        h6("inspect_na()"),
        tableOutput("inspectdf3"),
        
        h3("inspectdf"),
        h6("show_plot(inspect_na())"),
        plotOutput("inspectdf4", height = 400),
        
        h3("inspectdf"),
        h6("show_plot(inspect_cor())"),
        plotOutput("inspectdf5", height = 800),
        
        h3("DataExplorer"),
        h6("plot_correlation()"),
        plotlyOutput("DataExplorer", height = 800, width = 1200),
        
        h3("Highcharter"),
        h6("hchart()"),
        highchartOutput("Highcharter", height = 800, width = 1200),
        
        h3("corrmorant"),
        h6("ggcorrm()"),
        plotOutput("corrmorant", height = 1000, width = 1200),
        
        h3("skimr"),
        h6("skim()"),
        dataTableOutput("skim"),
        
        h3("naniar"),
        h6("gg_miss_upset()"),
        plotOutput("naniar", height = 600)
      )
    )))

server <- function(input, output) {
  rv <- reactiveValues()
  observe({
    rv$data_set <- data_list %>% pluck(input$dataset_choice)
  })
  
  output$Histogram <- renderPlot(
    rv$data_set %>% 
      mutate_if(is.character, as.factor) %>%
      mutate_if(is.factor, as.numeric) %>% 
      gather() %>% 
      ggplot(aes(x = value, group = key)) +
      geom_histogram(fill = "green", color = "darkgreen",bins = 30) +
      facet_wrap(~ key, ncol = 4, scale = "free"), res=96)
  
  output$inspectdf1 <- renderPlot(
    rv$data_set %>% inspect_num() %>% show_plot(), res=96)
  
  output$inspectdf2 <- renderPlot(
    rv$data_set %>% inspect_cat() %>% show_plot(), res=96)
  
  output$inspectdf3 <- renderTable(
    rv$data_set %>% inspect_na())
  
  output$inspectdf4 <- renderPlot(
    rv$data_set %>% inspect_na() %>% show_plot(), res=96)
  
  output$inspectdf5 <- renderPlot(
    rv$data_set %>% inspect_cor() %>% show_plot(), res=96)
  
  output$DataExplorer <- renderPlotly({
    g <- rv$data_set %>% plot_correlation()
    g %>% ggplotly()})
  
  output$Highcharter <- renderHighchart({
    col_names <- rv$data_set %>% 
      inspect_na() %>% 
      filter(pcnt < 50) %>% 
      pull(col_name)
    data <- rv$data_set %>% select(all_of(col_names))
    
    data %>% 
      mutate_if(is.character, as.factor) %>% 
      mutate_if(is.factor, as.numeric) %>%
      na.omit() %>% 
      cor() %>% 
      round(2) %>% 
      hchart(label = T)
    })
  
  output$corrmorant <- renderPlot(
    rv$data_set %>% 
      ggcorrm() +
      lotri(geom_point(alpha = 0.5)) +
      lotri(geom_smooth()) +
      utri_heatmap() +
      utri_corrtext() +
      dia_names(y_pos = 0.15, size = 3) +
      dia_histogram(lower = 0.3, fill = "grey80", color = 1) +
      scale_fill_corr() +
      labs(title = "Correlation Plot"), res=96)
  
  output$skim <- renderDataTable(
    rv$data_set %>% skim())
  
  output$naniar <- renderPlot(
    rv$data_set %>% gg_miss_upset(), res=96)
  }

shinyApp(ui,server)
