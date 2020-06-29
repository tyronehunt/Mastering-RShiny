library(shiny)

source("ch15_modules_example.R")


ui <- fluidPage(
  
  # Histogram Module
  histogramUI("hist1"),
  
  # Dataset Module
  datasetInput("dataset", filter = filter),
  tableOutput("data"),
  
  # Variable Module
  datasetInput("data", is.data.frame),
  selectVarInput("var"),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {
  # Histogram Module
  histogramServer("hist1")

  # Dataset Module
  data <- datasetServer("dataset")
  output$data <- renderTable(head(data()))
  
  # Variable Module
  data <- datasetServer("data")
  var <- selectVarServer("var", data)
  output$out <- renderPrint(var())
  
  # Histogram Module - 2
}

shinyApp(ui, server)  