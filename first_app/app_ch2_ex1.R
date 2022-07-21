library(shiny)

ui <- fluidPage(
  name <- textInput("name", "What's your name?"),
  textOutput("greeting")
  
)

server <- function(input, output, session) {
  
  output$greeting <- renderText({
    paste("Hello", input$name, sep=" ")
  })

}

# Run ---------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)