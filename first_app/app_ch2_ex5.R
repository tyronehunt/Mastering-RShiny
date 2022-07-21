library(ggplot2)
library(shiny)

datasets <- data(package = "ggplot2")$results[c(4), "Item"]

ui <- fluidPage(
  selectInput("dataset", "Dataset", choices = datasets),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  
  output$summary <- renderPrint({
    summary(dataset())
  })

  
  output$plot <- renderPlot({
    plot(dataset())
  })
}

# Run ---------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)