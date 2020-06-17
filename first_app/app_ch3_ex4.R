library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("superhero"),
                headerPanel("Central limit theorem"),
                sidebarLayout(position = c("right"),
                              sidebarPanel(
                                numericInput("m", "Number of samples:", 2, min = 1, max = 100)
                              ),
                              mainPanel(
                                plotOutput("hist")
                              )
                )
)

server <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  })
}
shinyApp(ui=ui, server=server)
