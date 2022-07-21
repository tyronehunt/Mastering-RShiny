library(shiny)

ui <- fluidPage(
  textOutput("text"),
  verbatimTextOutput("code"),
  
  tableOutput("static"),
  dataTableOutput("dynamic"),
  
  fluidRow(
    splitLayout(cellWidths = c("50%", "50%"),  plotOutput("plot1"),plotOutput("plot2"))
    )

)


server <- function(input, output, session) {
  output$text <- renderText("Hello friend!")
  output$code <- renderPrint(summary(1:5))
  
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 3))
  
  output$plot1 <- renderPlot(plot(1:5), res = 96)
  output$plot2 <- renderPlot(plot(1:5), res = 96)
  
}
# Run ---------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
