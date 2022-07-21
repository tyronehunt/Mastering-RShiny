library(DT)

# UI modules
sidebarCharts <- function(id) {
    ns <- NS(id)
    tagList(
        p(),
        actionButton(ns("settings"), "Settings", icon = icon("cogs"), width = '100%', class = "btn btn-info"),p(),
        actionButton(ns("refreshMainChart") ,"Refresh", icon("refresh"), width = '100%', class = "btn btn-primary"),p()
        ,textOutput(ns("info"))  # FOR DEBUGGING
    )
}

mainChartUI <- function(id) {
    ns <- NS(id)
    plotOutput(ns("mainChart"), width = "100%")
}

# UI module for the 2 buttons in the modal:
modalFooterUI <- function(ns) {
    tagList(
        modalButton("Cancel", icon("remove")),
        actionButton(ns("modalApply"), "Apply", icon = icon("check"))
    )
}

# module server
modal <- function(input, output, session) {
    
    # Init reactiveValues() to store default values and updated values; https://github.com/rstudio/shiny/issues/1588
    rv <- reactiveValues(clicks = 0, applyClicks = 0,
                         bins = 20,
                         bandwidth = 1)
    
    reactiveBlotter <- reactiveValues(df = NULL)  # Empty reactiveValues()
    
    # DEBUGGING
    output$info <- renderText({
        paste("You clicked the 'Settings' button", rv$clicks, "times. You clicked the 'Apply' button", rv$applyClicks, "times. The bin size is currently set to", rv$bins, "and the bandwidth is currently set to", rv$bandwidth)
    })
    
    settngsModal <- function(ns) {
        modalDialog(
            
            withTags({  # UI elements for the modal go in here
                fluidPage(
                    # titlePanel("Modal title optional"),
                    sidebarLayout(
                        sidebarPanel(
                            selectInput(ns("n_breaks"), label = "Number of bins:", choices = c(10, 20, 35, 50), selected = rv$bins, width = '100%'),
                            sliderInput(ns("bw_adjust"), label = "Bandwidth adjustment:", min = 0.2, max = 2, value = rv$bandwidth, step = 0.2, width = '100%'),
                            textInput(ns("testInputName"), label = "Name", width = '100%'),
                            selectInput(ns("testInput"), label = "Test Input", choices = c('A','B','C'), width = '100%'),
                            selectInput(ns("testInput2"), label = "Test Input 2", choices = c('D','E','F'), width = '100%'),
                            actionButton(ns("addToRV"), label = 'Add to reactiveValues'),p(),
                            actionButton(ns("clearRV"), label = 'Clear reactiveValues')
                        ),
                        mainPanel(
                            # "Blotter table goes here.",
                            fluidRow(
                                DT::DTOutput(ns("blotterDT"))
                            )
                        )
                    )
                )
            }),
            title = "Settings",
            footer = modalFooterUI(ns), 
            size = "l",
            easyClose = FALSE,
            fade = TRUE)
    }
    
    # Sidebar 'Settings' modal
    observeEvent(input$settings, {
        showModal(settngsModal(session$ns))  # This opens the modal; settngsModal() defined below
        rv$clicks <- rv$clicks + 1  # FOR DEBUGGING
    })
    
    observeEvent(input$modalApply, {
        rv$applyClicks <- rv$applyClicks + 1  # FOR DEBUGGING
        rv$bins <- input$n_breaks  # This is where I set the reactiveValues() to those inputted into the modal.
        rv$bandwidth <- input$bw_adjust
        removeModal()  # This should dismiss the modal (but it doesn't seem to work)
    })
    
    observeEvent(input$addToRV, { 
        req(input$testInputName, input$testInput, input$testInput2)
        
        reactiveBlotter[["df"]][[input$testInputName]] <- list(testInputName = input$testInputName, testInput = input$testInput, testInput2 = input$testInput2)
        
        
    })
    
    observeEvent(input$clearRV, {
        # reactiveBlotter <- reactiveValues()  # This doesn't work
        # lapply(X = names(reactiveBlotter), FUN = function(x) {
        #   reactiveBlotter[[x]] <- NULL
        # })  # This is one way to do it, but the names of the items within the reactiveValues() still remain.
        
        reactiveBlotter[["df"]] <- NULL
    })
    
    blotterDf <- reactive({
        do.call("rbind", lapply(reactiveBlotter[["df"]], FUN = function(x) {
            data.frame(name = x[["testInputName"]],
                       input1 = x[["testInput"]],
                       input2 = x[["testInput2"]])
        }))
    })
    
    output$blotterDT <- DT::renderDT({
        validate(need(length(names(reactiveBlotter[["df"]])) > 0, "Add something to the blotter."))
        DT::datatable(blotterDf(),
                      style = 'bootstrap',
                      class = 'table-bordered table-condensed',
                      rownames = TRUE,
                      options = list(dom = 't',
                                     paging = F)
        )
    })
    
    output$mainChart <- renderPlot({
        input$refreshMainChart  # Take dependency on the 'Refresh' buton
        
        hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(rv$bins),  
             xlab = "Duration (minutes)", main = "Geyser Eruption Duration")
        
        dens <- density(faithful$eruptions, adjust = rv$bandwidth)
        lines(dens, col = "blue")
    })
    
}

# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("stackoverflow example"),
    
    sidebarLayout(
        sidebarPanel(
            sidebarCharts("main")
        ),
        mainPanel(
            mainChartUI("main")
        )
    )
)

# Server logic
server <- function(input, output) {
    callModule(modal, "main")
}

# Complete app with UI and server components
shinyApp(ui, server)