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
                actionButton("add", "", 
                             icon("plus-circle fa-1x"), 
                             style="float:right; border:none; color:#00bc8c; background-color:rgba(0,0,0,0)"),
                tags$div(id = "add_UI_here")
            )
        ),
        column(width=8,
            actionButton(inputId = "print", label = "Show Table"),
            DT::dataTableOutput('preview_data')
        )
    
)

list_modules <- list()
current_id <- 1
filter_options <- c("cut", "color", "clarity")

server <- function(input, output, session) {
    
    # ADD UI ELEMENTS (MODULES)
    observeEvent(input$add, {
        
        new_id <- paste0("module_", current_id)
        
        list_modules[[new_id]] <<- callModule(module = module, id = new_id,
                       data = diamonds, variables = filter_options)
        
        insertUI(selector = "#add_UI_here",
                 # where = "afterEnd",
                 ui = module.UI(new_id, variables = filter_options))
        
        current_id <<- current_id + 1
        
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
                              autoWidth = TRUE, 
                              scrollX = TRUE, 
                              dom = 'ftpB',
                              pageLength = 4,
                              buttons = list(
                                  list(extend = "collection", text = 'Show More',
                                       action = DT::JS("function ( e, dt, node, config ) {dt.page.len(10); dt.ajax.reload();}")),
                                  list(extend = "collection", text = 'Show Less',
                                       action = DT::JS("function ( e, dt, node, config ) {dt.page.len(4); dt.ajax.reload();}"))
                              )))})
    })
    
       
}

# Run the application 
shinyApp(ui = ui, server = server)
