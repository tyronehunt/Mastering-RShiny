library(shiny)
library(shinydashboard)
library(fresh)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

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
    use_theme(mytheme),
    
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