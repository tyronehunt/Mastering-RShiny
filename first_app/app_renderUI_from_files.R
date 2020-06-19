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
    fileInput("oefile", "OE Mapping", placeholder = 'Select Open Ended (OE) .xlsx file.', accept = c(".xls", ".csv", ".xlsx")),
    fileInput("cefile", "CE Mapping", placeholder = 'Select Close Ended (CE) .xlsx file.', accept = c(".xls", ".csv", ".xlsx")),

    # Load files feedback
    textOutput("oe_feedback"),
    textOutput("ce_feedback"),
    
    # Buttons created on all files being uploaded
    uiOutput('ui.action'),
    uiOutput('ui.preview'),
    
    #Output table on preview button
    DT::dataTableOutput("head"),
    
    #
    textOutput('contents')
  )
  
  server <- function(input, output, session) {
    
    # Get file name when file is uploaded
    oe_file_name <-reactive(input$oefile$name)
    ce_file_name <-reactive(input$cefile$name)
    
    # Get file data when file_1 is uploaded
    data_1 <- reactive({
      req(input$oefile)
      ext <- tools::file_ext(oe_file_name())
      switch(ext,
             csv = read_csv(input$oefile$datapath),
             xlsx = read_excel(input$oefile$datapath),
             validate("Invalid file; Please upload a .csv or .xlsx file")
      )
    })
    
    # Get file data when file_2 is uploaded
    data_2 <- reactive({
      req(input$cefile)
      ext <- tools::file_ext(ce_file_name())
      switch(ext,
             csv = read_csv(input$cefile$datapath),
             xlsx = read_excel(input$cefile$datapath),
             validate("Invalid file; Please upload a .csv or .xlsx file")
      )
    })
    
    
    # Render UI button when data reactive is not null
    output$ui.action <- renderUI({
      if (is.null(data_1()) & is.null(data_2())) return()
      actionButton("action", "Run function")
    })
    
    # Run python script based on button input
    observeEvent(input$action, {
      source_python(here('hello_world.py'))
      output$contents <- renderPrint({"function running"})
    })
    
    # Create preview button
    output$ui.preview <- renderUI({
      if (is.null(data_1()) & is.null(data_2())) return()
      actionButton("preview", "Preview Table")
    })
    
    # output$contents <- renderPrint({
    #   if (is.null(input$action)) return(invisible(NULL))
    #   if (input$action==0) return()
    #   "this is action button text"
    # })
    
    # Output head of input data file, via data reactive
    observeEvent(input$preview, {
      output$head <- DT::renderDataTable({head(data_1())})
    })

    
    # Ouput file names uploaded, via oe_file_name reactive
    output$oe_feedback <-renderText({
      glue(oe_file_name(), " uploaded")
    })
    output$ce_feedback <-renderText({
      glue(ce_file_name(), " uploaded")
    })
    
  }
shinyApp(ui, server)