library(shiny)

ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),

sliderInput("y", label = "and y is", min = 1, max = 50, value = 5),
"then x times 5 is",
textOutput("product")
)

server <- function(input, output, session) {
  output$product <- renderText({ 
    input$x * input$y
  })
}

# Run ---------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)