module.UI <- function(id, variables){
  ns <- NS(id)
  
  ui = fluidRow(
    column(width=6,
        pickerInput(inputId = ns("picker_variable"),
                    choices = variables,
                    selected = NULL
        )
    ),
    column(width=6,
        pickerInput(inputId = ns("picker_value"),
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE
        )
    )
  )
}

module <- function(input, output, session, data, variables){
  
  observeEvent(input$picker_variable,{
    updatePickerInput(session,
                      inputId = "picker_value",
                      choices = as.character(unlist(unique(data[, input$picker_variable]))),
                      selected = NULL
    )
  })
  
  return(input)
}