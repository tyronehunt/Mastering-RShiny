library(glue)

puppies <- tibble::tribble(
  ~breed, ~ id, ~author, 
  "corgi", "corgi","alvannee",
  "labrador", "labrador", "shaneguymon",
  "spaniel", "spaniel", "_redo_"
)

ui <- fluidPage(
  selectInput("id", "Pick a breed", choices = setNames(puppies$id, puppies$breed)),
  htmlOutput("source"),
  imageOutput("photo")
)


server <- function(input, output, session) {
  
  output$photo <- renderImage({
    list(
      src = file.path("www", paste0(input$id, ".png")),
      width = 500,
      height = 650
    )
    
  }, deleteFile = FALSE)
  
  observeEvent(input$id, {
    message(glue("inputid: {input$id}"))
    t2 <- paste0(input$id, ".jpg")
    message(glue("second_part: {t2}"))
    t3 <- file.path("www", paste0(input$id, ".jpg"))
    message(glue("file_part: {t3}"))
  })
  
  output$source <- renderUI({
    info <- puppies[puppies$id == input$id, , drop = FALSE]
    HTML(glue::glue("<p>
      <a href='https://unsplash.com/photos/{info$id}'>original</a> by
      <a href='https://unsplash.com/@{info$author}'>{info$author}</a>
    </p>"))
  })
}
shinyApp(ui, server)