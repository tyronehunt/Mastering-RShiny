library(shiny)

ui <- fluidPage(
  fluidRow(
    actionButton("click", "Click me!", class = "btn-danger"),
    actionButton("drink", "Drink me!", class = "btn-lg btn-success")
  ),
  fluidRow(
    actionButton("eat", "Eat me!", class = "btn-block")
  ),
  textInput("userinput", "label_if_desired", placeholder = "Your name"),
  
  sliderInput("input_date", "Select Date", 
              min= as.Date("2015-01-01"), 
              max = as.Date("2015-07-01"), 
              value = as.Date("2015-04-01"), 
              timeFormat = "%F"),
  
  selectInput("state", "Choose a City:",
              list(`Kansas` = list("Overland Park", "Topeka", "Wichita"),
                   `Colorado` = list("Denver", "Boulder", "Colorado Springs"),
                   `Missouri` = list("St. Louis", "Clayton", "Columbia"))
  ),
  textOutput("result"), 
  
  sliderInput("user_input", "watch the automation",min = 0, max = 100, value  = 5,step = 5,animate = TRUE)
) 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    output$result <- renderText({
      paste("You chose", input$state)
    })
}
# Run ---------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
