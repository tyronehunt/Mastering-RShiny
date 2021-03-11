library(shiny)
library(shinyWidgets)
library(ggplot2)
library(magrittr)
library(dplyr)
library(DT)
library(shinydashboard)

# Import fontawesome icons
tags$head(tags$script(src="https://use.fontawesome.com/15c2608d79.js"))

###### SUB-MODULE
submodule_ui <- function(id, variables){
    # namespacing within submodule - not sub-module ui is namespaced, sub-module server is not
    ns <- NS(id)
    
    # Better to pass tagList back from submodules (to not clash with higher levels)
    tagList(
        fluidRow(
            column(width=6,
                   pickerInput(inputId = ns("picker_variable"),
                               choices = variables,
                               selected = NULL
                   )
            ),
            column(width=6,
                   pickerInput(inputId = ns("picker_value"),
                               choices = NULL,
                               selected = NULL,
                               multiple = TRUE
                   )
            )
        )
    )
}

# Note additional variables are passed into the main function, but NOT the moduleServer
submodule_server <- function(id, data, variables) {
    moduleServer(id, function(input, output, session){
        
        print("submodule server called")
        
        observeEvent(input$picker_variable,{
            
            print(head(data))
            print(variables)
            
            updatePickerInput(session,
                              inputId = "picker_value",
                              choices = as.character(unlist(unique(data[, input$picker_variable]))),
                              selected = NULL
            )
        })
        
        # We're returning the input of the submodule_server, i.e. all the reactives created
        # These are then accessed at module level in list_modules
        return(input)
    })}


###### MODULE
module_ui <- function(id){
    # As with submodule, there is namespacing at module level, only in module_ui, not module_server
    ns <- NS(id)
    
    # Note if you were adding sub-module ui elements you would do so here with: 
    # submodule_ui(ns("submodule_ui_1")), i.e. you would add the module namespacing to it's id
    # in this example, we're just using a div placeholder (which is separate from modules, so no ns())
    fluidPage(
        column(width=4,
               shinydashboard::box(title="Select Filter Options",
                                   actionButton(ns("removeBtn"), "", 
                                                icon("minus-circle fa-1x"), 
                                                style="float:left; border:none; color:#bc2c00; background-color:rgba(0,0,0,0)"),
                                   actionButton(ns("add"), "", 
                                                icon("plus-circle fa-1x"), 
                                                style="float:right; border:none; color:#00bc8c; background-color:rgba(0,0,0,0)"),
                                   fluidRow(),
                                   tags$div(id = "add_UI_here")
               )
        ),
        # As always outputs are also namespaced if they're within ui elements
        column(width=8,
               actionButton(inputId = ns("print"), label = "Show Table"),
               DT::dataTableOutput(ns('preview_data'))
        )
    )
}

module_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Note normally only ui is namespaced with ns = NS(id), but as the ui has already created
        # the ns for the module session, we just access it here. We only do so because we're creating
        # ui elements inside the server (with insertUI)
        ns <- session$ns   
        
        # Global variables defined here - these get passed down to submodule_server
        # List_modules allows you to access the various submodule_server returns
        list_modules <- list()
        filter_options <- c("cut", "color", "clarity")
        
        # ADD UI ELEMENTS (MODULES)
        observeEvent(input$add, {
            
            # We use the button clicks to increment a new sub-module server id
            btn <- input$add
            new_id <- paste0("module_", btn)
            
            print(paste0("new_id in module: ", new_id))
            list_modules[[new_id]] <<- submodule_server(new_id, diamonds, filter_options)
            
            # As creating ui element from the module server, by calling submodule_ui, we need ns
            insertUI(selector = "#add_UI_here",
                     # where = "afterEnd",
                     ui = tags$div(
                         submodule_ui(ns(new_id), filter_options),
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
        
        # We remove the most recently added sub-module using the id it was created with
        observeEvent(input$removeBtn, {
            print(paste0('#', names(list_modules)[length(list_modules)]))
            removeUI(
                selector = paste0('#', names(list_modules)[length(list_modules)])
            )
            list_modules <<- list_modules[-length(list_modules)]
        })
        
    })
}

###### APPLICATION
library(shiny)
app_ui <- function() {
    fluidPage(
        module_ui("module_ui_1")
    )
}

app_server <- function(input, output, session) {
    module_server("module_ui_1")
}

shinyApp(app_ui, app_server)