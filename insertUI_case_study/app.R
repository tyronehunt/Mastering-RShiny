library(shiny)
library(shinyWidgets)
library(ggplot2)
library(magrittr)
library(dplyr)
library(DT)
library(shinydashboard)

source('module.R')
tags$head(tags$script(src="https://use.fontawesome.com/15c2608d79.js"))

ui <- fluidPage(
        column(width=4,
            shinydashboard::box(title="Select Filter Options",
                # actionButton("removeBtn", "Remove"),
                actionButton("removeBtn", "", 
                             icon("minus-circle fa-1x"), 
                             style="float:left; border:none; color:#bc2c00; background-color:rgba(0,0,0,0)"),
                actionButton("add", "", 
                             icon("plus-circle fa-1x"), 
                             style="float:right; border:none; color:#00bc8c; background-color:rgba(0,0,0,0)"),
                fluidRow(),
                tags$div(id = "add_UI_here")
            )
        ),
        column(width=8,
            actionButton(inputId = "print", label = "Show Table"),
            DT::dataTableOutput('preview_data')
        )
    
)

list_modules <- list()
filter_options <- c("cut", "color", "clarity")

server <- function(input, output, session) {
    
    # ADD UI ELEMENTS (MODULES)
    observeEvent(input$add, {
        
        btn <- input$add
        new_id <- paste0("module_", btn)
        
        list_modules[[new_id]] <<- callModule(module = module, id = new_id,
                       data = diamonds, variables = filter_options)
        
        insertUI(selector = "#add_UI_here",
                 # where = "afterEnd",
                 ui = tags$div(
                        module.UI(new_id, variables = filter_options),
                        id = new_id
                      )
        )
        
    })
    
    # UPDATE DF BASED ON INSERTED UI
    observeEvent(input$print, {
        df <- diamonds
        
        # Filter data
        lapply(seq_len(length(list_modules)), function(i) {
            print(names(list_modules)[i])
            print(list_modules[[i]]$picker_variable)
            print(list_modules[[i]]$picker_value)
            df<<- df %>% filter(!!as.symbol(list_modules[[i]]$picker_variable) %in% list_modules[[i]]$picker_value)
        })
        
        # Plot data
        output$preview_data <- DT::renderDataTable({
            DT::datatable(df,
                          rownames = FALSE,
                          extensions = 'Buttons',
                          options = list(
                              paging = FALSE,
                              scrollX = TRUE, 
                              scrollY = "400px")
                          )})
    })
    
    
    observeEvent(input$removeBtn, {
        print(paste0('#', names(list_modules)[length(list_modules)]))
        removeUI(
            selector = paste0('#', names(list_modules)[length(list_modules)])
        )
        list_modules <<- list_modules[-length(list_modules)]
    })
       
}

# Run the application 
shinyApp(ui = ui, server = server)
