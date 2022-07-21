#' my_first_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_first_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("A plot"),
    plotOutput(ns("plot"))
  )
}
    
#' my_first_module Server Function
#'
#' @noRd 
mod_my_first_module_server <- function(input, output, session){
  ns <- session$ns
  output$plot <- renderPlot({
    x <- random_num()
    a <- golem::get_golem_options("a")
    plot(iris[, x*a])
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
