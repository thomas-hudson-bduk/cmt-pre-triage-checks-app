required_columns <-
  as.vector(read.csv(
    "required_columns.csv",
    header = FALSE,
    colClasses = "character"
  )$V1)

# Define the UI for a module
checks_ui <- function(id, label = "checks_ui") {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(selectInput(
      inputId = ns("selected_check"),
      label = "",
      choices = c(
        '1. Column names' = "selected_check_1",
        '2. Duplicate UPRNs' = "selected_check_2",
        '3. Current Technology' = "selected_check_3"
      )
    ),
    conditionalPanel(
      condition = "input.selected_check == 'selected_check_2'",
      selectInput(ns("column"), label = "Select a column:", choices = NULL)
    )
    ),
    mainPanel(
      verbatimTextOutput(ns("upload_status")),
      verbatimTextOutput(ns("check_1")),
      verbatimTextOutput(ns("check_2"))
    )
  )
}

# Define the server logic for a module
checks_server <- function(id, data, file_upload) {
  moduleServer(id,
               function(input, output, session) {
                 
                 observeEvent(file_upload(), {
                   updateSelectInput(session, "column", choices = colnames(data()))
                 })
                 
                 
                 output$upload_status <- renderPrint({
                   if (is.null(file_upload())) {
                     cat("No file uploaded.")
                   }
                 })
                 
                 output$check_1 <- renderPrint({
                   if (!is.null(file_upload()) &
                       input$selected_check == "selected_check_1") {
                     # Check if all the required columns exist in the data table
                     missing_columns <-
                       setdiff(tolower(required_columns), tolower(colnames(data())))
                     
                     # Output the result
                     if (length(missing_columns) == 0) {
                       cat("PASSED check 1. \nAll required columns are present.\n")
                     } else {
                       cat("FAILED check 1. \nThe following columns are missing:\n")
                       cat(paste0("-", missing_columns, "\n"))
                     }
                   }
                 })
                 
                 
                 output$check_2 <- renderPrint({
                   if (!is.null(file_upload()) & input$selected_check == "selected_check_2") {
                       req(input$column)
                       uprns <- data()[, input$column, drop = FALSE]
                       duplicate_count = sum(duplicated(na.omit(uprns)))
                       
                       # Output the result
                       if (duplicate_count == 0) {
                         cat("PASSED check 2. \nNo duplicate UPRNs are present.\n")
                       } else {
                         cat(
                           "FAILED check 2. \nThere are:",
                           format(duplicate_count, big.mark = ","),
                           "duplicates within the '", input$column, "' column of the uploaded data.\n"
                         )
                       }
                       
                     }
                 })
               })
  
}
