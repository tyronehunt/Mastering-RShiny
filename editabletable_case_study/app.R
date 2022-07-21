### Libraries
library(shiny)
library(dplyr)
library(DT)

### Data
input_data <- data.frame(Brand = c("Brand1", "Brand2","Brand3"),
                         ratio = c (.5, .5, .5),
                         cost = c(2000, 3000, 4000),
                         stringsAsFactors = FALSE) %>% 
    mutate(updated_price = cost * ratio)

### Module
modFunction <- function(input, output, session, data, reset) {
    
    v <- reactiveValues(data = data)
    
    proxy = dataTableProxy("mod_table")
    
    observeEvent(input$mod_table_cell_edit, {
        print(names(v$data))
        info = input$mod_table_cell_edit
        str(info)
        i = info$row
        j = info$col
        k = info$value
        str(info)
        
        isolate(
            if (j %in% match(c("ratio","cost","updated_price"), names(v$data))) {
                print(match(c("ratio","cost", "updated_price"), names(v$data)))
                v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
                print(v$data)
                
                if (j %in% match("cost", names(v$data))) {
                    v$data$updated_price <<- v$data$cost * v$data$ratio
                }
                if (j %in% match("ratio", names(v$data))) {
                    v$data$updated_price <<- v$data$cost * v$data$ratio
                }
            } else {
                stop("You are not supposed to change this column.") # check to stop the user from editing only few columns
            }
        )
        replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
    })
    
    ### Reset Table
    observeEvent(reset(), {
        v$data <- data # your default data
    })
    
    print(isolate(colnames(v$data)))
    output$mod_table <- DT::renderDataTable({
        DT::datatable(v$data, editable = TRUE)
        
    })
}

modFunctionUI <- function(id) {
    ns <- NS(id)
    DT::dataTableOutput(ns("mod_table"))
    
}

### Shiny App
shinyApp(
    ui = basicPage(
        mainPanel(
            
            actionButton("reset", "Reset"),
            tags$hr(),
            modFunctionUI("editable")
        )
    ),
    server = function(input, output) {
        demodata<-input_data
        callModule(modFunction,"editable", demodata,
                   reset = reactive(input$reset))
        
    }
)