# THE FOLLOWING IS AN APP TEMPLATE USING SHINYDASHBOARD - ACCOMPANIED BY A CUSTOM CSS SHEET, OR IF YOU PREFER FRESH LIBRARY STYLING
# THE TEMPLATE IS A SIMPLE 2 TAB SYSTEM, EACH WITH MULTIPLE FILE UPLOADS AND REACTIVES TO GUIDE USER FLOW FROM UPLOAD TO DATA PREVIEW
# PYTHON RETICULATE FUNCTIONS AND DOWNLOAD

library(shiny)
library(shinydashboard)
library(tidyverse)
library(reticulate)
library(readxl)
library(openxlsx)
library(DT)
library(glue)
library(here)
library(fresh)
#Use these lines to reset here() if running locally
# detach("package:here", unload=TRUE)
# setwd("/Users/name/Documents/directory_name")


# Set maximum file limit size (up from 5mb). I.e. 30mb
options(shiny.maxRequestSize = 30*1024^2)

# You can use fresh library to create css theme as below, or manually with css file in www folder. 
# mytheme <- create_theme(
#   adminlte_color(
#     light_blue = "#0a9dc8"
#   ),
#   adminlte_sidebar(
#     width = "200px",
#     dark_bg = "#363636",
#     dark_hover_bg = "#f8f4d6",
#     dark_color = "#ababab"
#   ),
#   adminlte_global(
#     content_bg = "#ceebf4"
#   )
# )

# UI ----------------------------------------------------------------------
ui <- dashboardPage(skin = "blue",
  # Set header and logo (html inline)
  
  # Custom HTML inserted straight into page here. 
  dashboardHeader(title = tags$div(tags$img(src='left_logo.png', height='40', width='40',
                                            style =  "margin:5px; float:left"),
                                   "Header Title"),
                  tags$li(a(href = '/',
                            img(src = 'right_logo.png',
                                title = "Company Home", height = "40px"),
                            style = "padding-top:5px; padding-bottom:5px;"),
                          class = "dropdown")
  ),
  
  # UI Sidebar --------------------------------------------------------------
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tab 1 Title", tabName = "tab_1", icon = icon("star")),
      menuItem("Tab 2 Title", tabName = "tab_2", icon = icon("th"))
    )  
  ),
  
  # UI Body -----------------------------------------------------------------
  ## Body content
  dashboardBody(
    
    # Reference www/custom.css file
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    # If you were using fresh, place line below:
    # use_theme(mytheme),
    
    
    # UI - clean data ---------------------------------------------------------
    tabItems(
      # First tab content
      tabItem(tabName = "tab_1",
        fluidRow(title = "Title of fluidRow",
           column(width = 4,
              h4("Please upload the following files:"),
              fileInput('file1', "File1 Label",
                        placeholder = 'file_1.xlsx',
                        accept=c('.xls', '.xlsx', '.csv')),
              htmlOutput("f1_feedback"),
              
              fileInput('file2', "File2 Label",
                        placeholder = 'file_2.xlsx',
                        accept=c('.xls', '.xlsx', '.csv')),
              htmlOutput("f2_feedback"),
              
              fileInput('file3', "File3 Label",
                        placeholder = 'file_3.xlsx',
                        accept=c('.xls', '.xlsx', '.csv')),
              htmlOutput("f3_feedback"),
              
              # Download Cleaned Data File
              fluidRow(column(width = 6, uiOutput('ui.functionbutton')),
                       column(width = 6, uiOutput('ui.preview'))),
              fluidRow(column(width = 6, tags$br(),
                              uiOutput('ui.download_button_name')),
                       column(width = 6))
           ),
           
           column(width = 8,
              # Preview Results
              DT::dataTableOutput("table_clean")
            )
      )
    ),
      
      
      # UI - analyze data -------------------------------------------------------
      # Second tab content
      tabItem(tabName = "tab_2",
          fluidRow(title = "fluidRow title",
               column(width = 4,
                      h4("Please upload the following files:"),
                      
                      fileInput('file4','File 4 Label',
                                placeholder = 'file4.xlsx',
                                accept=c('.xls', '.xlsx', '.csv')),
                      htmlOutput("f4_feedback"),
                      
                      fileInput('templatefile', 'User input template',
                                accept=c('.xls', '.xlsx', '.csv')),
                      htmlOutput("template_feedback"),
                      
                      radioButtons("analysis_type", "Analysis Type:",
                                   choiceNames = list(
                                     "Analysis Type 1",
                                     "Analysis Type 2",
                                     "Analysis Type 3"),
                                   choiceValues = list("analysis_1", "analysis_2", "analysis_3")
                      ),
                      
                      # Analyze Data
                      fluidRow(column(width = 6, uiOutput('ui.analyze')),
                               column(width = 6, uiOutput('ui.preview2'))),
                      fluidRow(column(width = 6, tags$br(), uiOutput('ui.results_dataset')),
                               column(width = 6))
               ),
               
               column(width = 8,
                      # Preview Results
                      DT::dataTableOutput("results_table")
               )
          )
      )
)))

server <- function(input, output) {
  # SERVER - CLEAN DATA -----------------------------------------------------
  # Get file names when they are uploaded, pass to reactives
  f1_file_name <-reactive(input$file1$name)
  f2_file_name <-reactive(input$file2$name)
  f3_file_name <-reactive(input$file3$name)
  
  # Load data files into reactives
  data_1 <- reactive({
    req(input$file1)
    file1_df <<- read_excel(input$file1$datapath)
  })
  data_2 <- reactive({
    req(input$file2)
    file2_df <<- read_excel(input$file2$datapath)
  })
  data_3 <- reactive({
    req(input$file3)
    file3_df <<- read_excel(input$file3$datapath)
  })
  
  # Feedback to user files uploaded
  output$f1_feedback <-renderText(glue("<i>Uploaded: ",f1_file_name(), "<br><br></i>"))
  output$f2_feedback <-renderText(glue("<i>Uploaded: ",f2_file_name(), "<br><br></i>"))
  output$f3_feedback <-renderText(glue("<i>Uploaded: ",f3_file_name(), "<br><br></i>"))
  
  # Render UI download button when all data uploaded
  output$ui.functionbutton <- renderUI({
    if (is.null(data_1()) & is.null(data_2()) & is.null(data_3())) return()
    actionButton("action", "Clean Data", icon = icon("play-circle"))
  })
  
  # Run python cleaning script based on action button input
  observeEvent(input$action, {
    source_python(here('hello_world.py'))
    
    #Clean data.frame column types (or any other cleaning step required as a result of reticulate troubles)
    python_df <<- data.frame(lapply(python_df, function(x) if(class(x) == 'list') {
      return(unlist(x))
    } else {
      return(x)
    }
    ), stringsAsFactors=FALSE)
    
    # Create preview button (based on clean data button)
    output$ui.preview <- renderUI({
      actionButton("preview", "Preview", icon = icon("table"))
    })
    
    # Create download button (based on clean data button)
    output$ui.download_button_name <- renderUI({
      downloadButton("download_button_name", "Download")
    })
  })
  
  # Preview cleaned data file, based on preview button
  observeEvent(input$preview, {
    output$table_clean = DT::renderDataTable({
      DT::datatable(
        python_df,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          autoWidth = TRUE, 
          scrollX = TRUE, 
          # columnDefs = list(list(width = '250px', targets = c(3,5,6))),
          dom = 'ftpB',
          pageLength = 5,
          buttons = list(
            list(extend = "collection", text = 'Show More',
                 action = DT::JS("function ( e, dt, node, config ) {dt.page.len(20); dt.ajax.reload();}")),
            list(extend = "collection", text = 'Show Less',
                 action = DT::JS("function ( e, dt, node, config ) {dt.page.len(5); dt.ajax.reload();}"))
          )))})
  })
  
  #Download file
  output$download_button_name <- downloadHandler(
    filename = 'output_file_name.csv',
    content = function(file){
      write.csv(python_df, file)
    })
  
  
  # SERVER - ANALYZE DATA ---------------------------------------------------
  # Get file names when they are uploaded, pass to reactives
  template_file_name <-reactive(input$templatefile$name)
  responses_file2_name <-reactive(input$file4$name)
  
  # Load data files into reactives
  data_4 <- reactive({
    req(input$templatefile)
    templatefile_df <<- read_excel(input$templatefile$datapath, col_types = "text")
  })
  
  data_5 <- reactive({
    req(input$file4)
    file4_df <<- read_excel(input$file4$datapath)
    # file4_df <<- read.csv(file4$datapath, stringsAsFactors = FALSE, na="")
  })
  
  # Feedback to user files uploaded
  output$f4_feedback <-renderText(glue("<i>Uploaded: ",responses_file2_name(), "<br><br></i>"))
  output$template_feedback <-renderText(glue("<i>Uploaded: ",template_file_name(), "<br><br></i>"))  
  
  # Render clean data button when all data uploaded
  output$ui.analyze <- renderUI({
    if (is.null(data_4()) & is.null(data_5())) return()
    actionButton("analyze", "Analyze Data", icon = icon("play-circle"))
  })
  
  # Run python cleaning script based on analyze button input
  observeEvent(input$analyze, {
    
    analysis_type <<- input$analysis_type
    source_python(here('hello_world.py'))
    
    # Create preview button (based on clean data button)
    output$ui.preview2 <- renderUI({
      actionButton("preview2", "Preview", icon = icon("table"))
    })
    
    # Create download button (based on clean data button)
    output$ui.results_dataset <- renderUI({
      downloadButton("results_dataset", "Download")
    })
  })
  
  # Preview results data file, based on preview2 button
  observeEvent(input$preview2, {
    output$results_table = DT::renderDataTable({
      DT::datatable(
        results_summary_df,
        rownames = TRUE,
        extensions = 'Buttons',
        options = list(
          autoWidth = TRUE, 
          scrollX = TRUE, 
          # columnDefs = list(list(width = '250px', targets = c(3,5,6))),
          dom = 'ftpB',
          pageLength = 5,
          buttons = list(
            list(extend = "collection", text = 'Show More',
                 action = DT::JS("function ( e, dt, node, config ) {dt.page.len(20); dt.ajax.reload();}")),
            list(extend = "collection", text = 'Show Less',
                 action = DT::JS("function ( e, dt, node, config ) {dt.page.len(5); dt.ajax.reload();}"))
          )))})
  })
  
  
  #Download file
  output$results_dataset <- downloadHandler(
    filename = 'final_output.csv',
    content = function(file){
      write.csv(results_summary_df, file)
    })
  
 
  
}
# RUN APP -----------------------------------------------------------------
shinyApp(server = server, ui = ui)