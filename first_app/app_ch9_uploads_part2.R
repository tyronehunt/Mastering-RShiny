library(shiny)
library(glue)
library(readxl)
library(readr)
options(shiny.maxRequestSize = 10 * 1024^2)

ui <- fluidPage(
  fileInput("file", NULL, accept = c(".csv", ".xlsx")),
  numericInput("n", "Rows", value = 3, min = 1, step = 1),
  tableOutput("head")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = read_csv(input$file$datapath),
           xlsx = read_excel(input$file$datapath),
           validate("Invalid file; Please upload a .csv or .xlsx file")
    )
    
  })
  
  output$head <- renderTable({
    head(data(), input$n)
  })
}


shinyApp(ui, server)