#' name_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_name_of_module1_ui <- function(id){
  ns <- NS(id)
  tagList(
  h2("hello world"),
  sliderInput(ns("n"), "Nrow", 1, 50, 25),
  DT::dataTableOutput(ns("dt"))
  )
}
    
#' name_of_module1 Server Functions
#'
#' @noRd 
mod_name_of_module1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  output$dt <- DT::renderDataTable({
    my_dataset[
      1:input$n
    ]
  })
  })
}
    
## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_ui_1")
    
## To be copied in the server
# mod_name_of_module1_server("name_of_module1_ui_1")
