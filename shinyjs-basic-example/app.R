library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fixedPage(
  shinydisconnect::disconnectMessage2(),
  useShinyjs(),
  inlineCSS(list(.big = "font-size: 2em",
                 a = "cursor: pointer")),
  fixedRow(
    column(6, wellPanel(
      div(id = "myapp",
          h2("shinyjs demo"),
          checkboxInput("big", "Bigger text", FALSE),
          textInput("name", "Name", ""),
          a(id = "toggleAdvanced", "Show/hide advanced info"),
          hidden(
            div(id = "advanced",
                numericInput("age", "Age", 30),
                textInput("company", "Company", "")
            )
          ),
          p("Timestamp: ",
            span(id = "time", date()),
            a(id = "update", "Update")
          ),
          actionButton("submit", "Submit"),
          actionButton("reset", "Reset form")
      ),
      br(), br()
    )),
    column(6,
        # This is copied from https://github.com/daattali/shinyjs/blob/master/inst/examples/basic/helper-text.R
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #When the id="update" is clicked, change it to the html with id=time and content=date()
  onclick("update", html("time", date())) 
  #When the id="toggleAdvanced" is clicked, toggle from hidden to show id=advanced with an animation
  onclick("toggleAdvanced", toggle(id = "advanced", anim = TRUE))
  
  # Toggle the class of the div with id=myapp to have class=.big as specified in inlineCSS, based on condition input%big
  observe({
    toggleClass("myapp", "big", input$big)
  })
  
  #Toggle the state of button with id=submit if condition is met that input$name is not empty
  observe({
    toggleState("submit", !is.null(input$name) && input$name != "")
  })
  
  #Alert on action
  observeEvent(input$submit, {
    alert("Thank you!")
  })
  
  # Reset elements in the div with id=myapp back to their original values
  observeEvent(input$reset, {
    reset("myapp")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
