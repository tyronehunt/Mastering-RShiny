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
  
  print("submodule server called")
  print(input)
  print("output!")
  print(output)
  print("end of output!")
  
  observeEvent(input$picker_variable,{
    
    print(head(data))
    print(variables)
    
    updatePickerInput(session,
                      inputId = "picker_value",
                      choices = as.character(unlist(unique(data[, input$picker_variable]))),
                      selected = NULL
    )
  })
  
  return(input)
}