library(shiny)


# Histogram Module --------------------------------------------------------
histogramUI <- function(id) {
  tagList(
    selectInput(NS(id, "var"), "Variable", names(mtcars)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}


histogramServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
  })
}


# Dataset Module --------------------------------------------------------
datasetInput <- function(id, filter = NULL) {
  names <- ls("package:datasets")
  selectInput(NS(id, "dataset"), "Pick a dataset", choices = names)
}

# Note how the module server can be assigned a value, in this case the name of the dataset from get
datasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(get(input$dataset, "package:datasets"))
  })
}


# Case study - Numeriuc Variable ------------------------------------------------------
selectVarInput <- function(id) {
  selectInput(NS(id, "var"), "Variable", choices = NULL) 
}

find_vars <- function(data, filter) {
  names(data)[vapply(data, filter, logical(1))]
}

selectVarServer <- function(id, data, filter = is.numeric) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "var", choices = find_vars(data(), filter))
    })
    
    reactive(data()[[input$var]])
  })
}
