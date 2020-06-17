library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Dashboard Title"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tab Title 1", tabName = "tab_1", icon = icon("star")),
      menuItem("Table Title 2", tabName = "tab_2", icon = icon("th"))
    )  
  ),
  
  ## Body content
  dashboardBody(
    
    # css here (referencing a file in www subdirectory)
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css")
    ),
    
    tabItems(
      # First tab content
      tabItem(tabName = "tab_1",
              fluidRow(
                box("content in box here", height=150),
                
                box(title = "Box Title","more content here", height=150)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "tab_2",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  

}

shinyApp(ui, server)