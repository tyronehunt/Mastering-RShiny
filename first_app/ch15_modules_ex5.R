library(shiny)
library(shinipsum)
library(DT)
ui <- fluidPage(
  h2("A Random DT"),
  DTOutput("data_table"),
  h2("A Random Plot"),
  plotOutput("plot"),
  h2("A Random Text"),
  tableOutput("text"),
  h2("A Random Table"),
  tableOutput("table"),
  random_print(type = "model")
)

server <- function(input, output, session) {
  output$data_table <- DT::renderDT({
    random_DT(5, 5)
  })
  output$plot <- renderPlot({
    random_ggplot()
  })
  output$text <- renderText({
    random_text(nwords = 50)
  })
  output$table <- renderTable({
    random_table(nrow = 3, ncol = 10)
  })
  

}
shinyApp(ui, server)