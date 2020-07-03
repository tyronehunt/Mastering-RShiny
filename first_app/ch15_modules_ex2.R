# Re-usable module
mod_ui <- function(id, button_label) {
    ns <- NS(id)
    tagList(
        sliderInput(
            inputId = ns("choice"), 
            label = "Choice",
            min = 1, max = 10, value = 5
        ),
        actionButton(ns("validate"), button_label)
    )
}

mod_server <- function(id) {
    moduleServer(id, function(input, output, session) {

    observeEvent( input$validate , {
        print(input$choice)
    })
    })
}


# Main application
library(shiny)
app_ui <- function() {
    fluidPage(
        mod_ui("mod_ui_1", button_label = "Validate Choice"),
        mod_ui("mod_ui_2", button_label = "Validate Choice, again")
    )
}

app_server <- function(input, output, session) {
    mod_server(id = "mod_ui_1")
    mod_server(id = "mod_ui_2")
}

shinyApp(app_ui, app_server)