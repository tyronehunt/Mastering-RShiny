#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_my_first_module_server, "my_first_module_ui_1")
  callModule(mod_my_other_module_server, "my_other_module_ui_1")
  
  observeEvent(input$alert, {
    golem::invoke_js("alert", "Yeay!")
  })
  print(
    golem::get_golem_options("a")
  )

}
