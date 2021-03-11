############## APP 1 - PRE REFORMAT ########################
library(shiny)
library(ggplot2)

testPlotUI <- function(id, stringName){
    ns <- NS(id)
    fluidRow(
        column(12,
               plotOutput(ns(stringName))
        )
    )
}

testPlot <- function(input, output, session, data, xAxis, yAxis){
    output$test <- renderPlot({
        ggplot(data(), aes_string(x=xAxis(), y=yAxis())) + geom_bar(stat = "identity")
    })
}

# create data
name <- c("Moller", "Mayer", "Bernard")
sales <- c(35000, 40000, 60000)
df <- data.frame(name, sales)

# app
server <- function(input, output, session) {
    x <- callModule(testPlot, "test", data = reactive(df), xAxis = reactive("name"), yAxis = reactive("sales"))
}
ui <- fluidPage(
    testPlotUI(id = "test", stringName = "test")
)

shinyApp(ui = ui, server = server)
############################# APP 1 - POST REFORMAT 
# Sub module:
testPlotUI <- function(id, stringName){
    ns <- NS(id)
    tagList(
        plotOutput(ns(stringName))
    )
}

testPlot <- function(id, data, xAxis, yAxis){
    moduleServer(id, function(input, output, session) {
        output$test <- renderPlot({
            ggplot(data(), aes_string(x=xAxis(), y=yAxis())) + geom_bar(stat = "identity")
        })
    })
}

# Module:
mod_tabowl_support_ui <- function(id){
    ns <- NS(id)
    tagList(
        testPlotUI(id = ns("test"), stringName = "test")
    )
}

mod_tabowl_support_server <- function(id){
    moduleServer( id, function(input, output, session){

        # create data
        name <- c("Moller", "Mayer", "Bernard")
        sales <- c(35000, 40000, 60000)
        df_test <- data.frame(name, sales)

        x <- testPlot(id="test", data = reactive(df_test),
                      xAxis = reactive("name"), yAxis = reactive("sales")
        )
    })
}

# Application
library(shiny)
app_ui <- function() {
    fluidPage(
        mod_tabowl_support_ui("tabowl_support_ui_1")
    )
}

app_server <- function(input, output, session) {
    mod_tabowl_support_server("tabowl_support_ui_1")
}

shinyApp(app_ui, app_server)

############################# APP 2 - POST TRANSFORM
# Sub-module
mod_b_ui <- function(id) {
    ns <- NS(id)
    tagList(
        actionButton(ns("validate"), "Print")
    )
}

mod_b_server <- function(id, react) {
    moduleServer(id, function(input, output, session) {
        observeEvent( input$validate , {
            print(react())
        })
    })
}

# Module
mod_ui <- function(id) {
    ns <- NS(id)
    tagList(
        sliderInput(ns("choice"), "Choice", 1, 10, 5),
        mod_b_ui(ns("mod_ui_2"))
    )
}

mod_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        res <- reactive({input$choice})
        mod_b_server("mod_ui_2", react = res)
    })
}

# Application
library(shiny)
app_ui <- function() {
    fluidPage(
        mod_ui("mod_ui_1")
    )
}

app_server <- function(input, output, session) {
    res <- mod_server("mod_ui_1")
}

shinyApp(app_ui, app_server)