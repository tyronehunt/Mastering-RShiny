library(shiny)
library(shinyWidgets)
library(ggplot2)
library(magrittr)
library(dplyr)
library(DT)

source('module.R')

ui <- fluidPage(
    
        actionButton(inputId = "print", label = "print inputs"),
        actionButton(inputId = "add",
                     label = "Add"),
        tags$div(id = "add_UI_here"),
        DT::dataTableOutput('preview_data')
    
)

list_modules <- list()
current_id <- 1

server <- function(input, output, session) {
    
    observeEvent(input$add, {
        
        new_id <- paste0("module_", current_id)
        
        list_modules[[new_id]] <<- callModule(module = module, id = new_id,
                       data = diamonds, variables = c("cut", "color", "clarity"))
        
        insertUI(selector = "#add_UI_here",
                 ui = module.UI(new_id, variables = c("cut", "color", "clarity")))
        
        current_id <<- current_id + 1
        
    })
    
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
