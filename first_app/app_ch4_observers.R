library(shiny)

ui <- fluidPage(
  fluidRow(
    column(3, 
           textInput("name", "What's your name?"),
           textOutput("greeting")
    )
  )
)

server <- function(input, output, session) {
  
  text <- reactive(paste0("Hello ", input$name, "!"))
  
  # output$greeting <- renderText(text())
  observeEvent(input$name, {
    message("Greeting performed")
  })
  
}
shinyApp(ui, server)