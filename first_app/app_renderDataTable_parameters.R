library(shiny)
library(shinydashboard)
library(tidyverse)
library(reticulate)
library(readxl)
library(openxlsx)
library(DT)

#Use these lines to reset here() if running locally
# detach("package:here", unload=TRUE)
# setwd("/Users/tyrone/Documents/Tabowl")
library(here)

# Set maximum file limit size (up from 5mb)
options(shiny.maxRequestSize = 30*1024^2)



ui <- fluidPage(

fileInput('oefile',
         'Please upload OE (open ended) excel file.',
         accept=c('.xls', '.xlsx', '.csv')),

# Preview Results
DT::dataTableOutput("table_clean")

)

server <- function(input, output) {

oe_file_name <-reactive(input$oefile$name)

data_1 <- reactive({
    req(input$oefile)
    ext <- tools::file_ext(oe_file_name())
    switch(ext,
           csv = read_csv(input$oefile$datapath),
           xlsx = read_excel(input$oefile$datapath),
           validate("Invalid file; Please upload a .csv or .xlsx file")
    )
  })

output$table_clean = DT::renderDataTable({
  DT::datatable(
    data_1(),
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
      autoWidth = TRUE, 
      scrollX = TRUE, 
      # columnDefs = list(list(width = '250px', targets = c(3,5,6))),
      dom = 'tpB',
      pageLength = 5,
      buttons = list(
        list(extend = "collection", text = 'Show More',
             action = DT::JS("function ( e, dt, node, config ) {dt.page.len(20); dt.ajax.reload();}")),
        list(extend = "collection", text = 'Show Less',
             action = DT::JS("function ( e, dt, node, config ) {dt.page.len(5); dt.ajax.reload();}"))
)))})

}
# RUN APP -----------------------------------------------------------------
shinyApp(server = server, ui = ui)