# Module 1
mod_ui <- function(id) {
    ns <- NS(id)
    tagList(
        sliderInput(ns("choice"), "Choice", 1, 10, 5)
    )
}

mod_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        observeEvent( input$choice , {
            r$choice <- input$choice
        })
    })
}


# Module 2
mod_b_ui <- function(id) {
    ns <- NS(id)
    tagList(
        actionButton(ns("validate"), "Print")
    )
}

mod_b_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        observeEvent( input$validate , {
            print(r$choice)
        })
    })
}


# Application
library(shiny)
app_ui <- function() {
    fluidPage(
        mod_ui("mod_ui_1"),
        mod_b_ui("mod_ui_2")
    )
}

app_server <- function(input, output, session) {
    r <- reactiveValues()
    mod_server("mod_ui_1", r)
    mod_b_server("mod_ui_2", r)
}

shinyApp(app_ui, app_server)