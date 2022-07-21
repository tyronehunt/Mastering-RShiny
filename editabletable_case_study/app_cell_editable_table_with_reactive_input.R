# Module
mod_ui <- function(id) {
    ns <- NS(id)
    tagList(
        actionButton(ns("analyzeBtn"), "Results"),
        actionButton(ns("reset"), "Reset"),
        tags$hr(),
        DT::dataTableOutput(ns("mod_table"))
    )
}

mod_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        v <- reactiveValues()
        
        observeEvent(input$analyzeBtn, {
            whereami::cat_where(whereami::whereami())
            a_string <- 'some string'
            dta <<- data.frame(boundary_words=c("one", "two"), stringsAsFactors = FALSE) %>% dplyr::mutate(replace = 'F')
            v$dta <- dta
            v$some_string <- a_string
        })
        
        proxy = dataTableProxy("mod_table")
        
        observeEvent(input$mod_table_cell_edit, {
            whereami::cat_where(whereami::whereami())
            print(names(v$dta))
            info = input$mod_table_cell_edit
            str(info)
            i = info$row
            j = info$col
            k = info$value
            
            # Modify data if column index = 2
            if (j == 2){
                isolate(
                    v$dta[i, j] <<- DT::coerceValue(k, v$dta[i, j])
                )
                print("V dta")
                print(v$dta)
                replaceData(proxy, v$dta, resetPaging = FALSE)  # replaces data displayed by the updated table
            
                # Set list of words to replace with correct version in dataframe
                dta_test_list <<- v$dta %>% dplyr::filter(replace == 'T')
                dta_replace_list <<- as.list(v$dta %>% dplyr::filter(replace == 'T') %>% select(boundary_words))
                print(dta_replace_list)
                
            # Prevent user from modifying column they're not supposed to
            } else {
                isolate(
                    v$dta[i, j] <<- DT::coerceValue(dta[i, j], v$dta[i, j])
                )
                replaceData(proxy, v$dta, resetPaging = FALSE)
            }
        })
        
        ### Reset Table
        observeEvent(input$reset, {
            v$dta <- dta # your default data
        })
        
        output$mod_table <- DT::renderDataTable({
            DT::datatable(v$dta, editable = TRUE)
        })
        
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
    mod_server("mod_ui_1")
}

shinyApp(app_ui, app_server)