library(shiny)

ui <- fluidPage(
  sliderInput("x", "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", "and y is", min = 1, max = 50, value = 5),
  "then, (x * y) is", textOutput("product1"),
  "and, (x * y) + 5 is", textOutput("product_plus5"),
  "and (x * y) + 10 is", textOutput("product_plus10")
)

server <- function(input, output, session) {
  product <- reactive({ input$x * input$y })
  
  output$product <- renderText({ 
    product()
  })
  
  output$product_plus5 <- renderText({ 
    product() + 5
  })
  
  output$product_plus10 <- renderText({ 
    product() + 10
  })
}

# Run ---------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)