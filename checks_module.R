# Define the UI for a module
checksUI <- function(id, label = "Checks") {
  ns <- NS(id)
  sidebarLayout(sidebarPanel(
    selectInput(
      inputId = "selected_check",
      label = "",
      choices = c(
        "All",
        "1. Column names",
        "2. Duplicate UPRNs",
        "3. Current Technology"
      )
    )
  ),
  mainPanel(
    verbatimTextOutput("upload_status")
  ))
}

# Define the server logic for a module
checksServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$upload_status <- renderPrint({
        if (is.null(input$file_upload)) {
          cat("No file uploaded.")
        }
      })
      
    }
  )
}
