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
    sidebarPanel(
      selectInput(
        inputId = ns("selected_check"),
        label = "",
        choices = c("1. Column names",
                    "2. Duplicate UPRNs",
                    "3. Current Technology")
      ),
      conditionalPanel(
        condition = "input.selected_check == `2. Duplicate UPRNs`",
        selectInput("column", label = "Select a column:", choices = NULL)
      )
    ),
    mainPanel(
      verbatimTextOutput(ns("upload_status")),
      verbatimTextOutput(ns("check_1")),
      verbatimTextOutput(ns("check_2")),
      verbatimTextOutput(ns("check_3"))
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
                       input$selected_check == "1. Column names") {
                     # Check if all the required columns exist in the data table
                     missing_columns <-
                       setdiff(tolower(required_columns), tolower(colnames(data())))
                     
                     # Output the result
                     if (length(missing_columns) == 0) {
                       cat("PASSED check 1. \n\nAll required columns are present.\n")
                     } else {
                       cat("FAILED check 1. \n\nThe following columns are missing:\n")
                       cat(paste0("-", missing_columns, "\n"))
                     }
                   }
                 })
                 
                 
                 output$check_2 <- renderPrint({
                   if (!is.null(file_upload()) &
                       input$selected_check == "2. Duplicate UPRNs") {
                     uprns <- data()[, 1, drop = FALSE]
                     duplicate_count = sum(duplicated(na.omit(uprns)))
                     
                     # Output the result
                     if (duplicate_count == 0) {
                       cat("PASSED check 2. \n\nNo duplicate UPRNs are present.\n")
                     } else {
                       cat(
                         "FAILED check 2. \n\nThere are: ",
                         format(duplicate_count, big.mark = ","),
                         " duplicates within the '",
                         colnames(data())[[1]],
                         "' column of the uploaded data.\n",
                         sep = ""
                       )
                     }
                     
                   }
                 })
                 
                 
                 output$check_3 <- renderPrint({
                   if (!is.null(file_upload()) &
                       input$selected_check == "3. Current Technology") {
                     data = data()
                     check_3_1 = 0
                     check_3_2 = 0
                     check_3_3 = 0
                     
                     if (all(is.numeric(data$current_max_download_speed), na.rm = TRUE)) {
                       check_3_1 = 1
                     }
                     
                     if (all(data$current_max_download_speed > 0, na.rm = TRUE)) {
                       check_3_2 = 1
                     }
                     
                     if (any(data$current_technology == "FTTP") &&
                         all(data$current_max_download_speed[data$current_technology == "FTTP"] >= 1000, na.rm = TRUE)) {
                       check_3_3 = 1
                     }
                     
                     if (check_3_1 + check_3_2 + check_3_3 == 3) {
                       cat("PASSED check 3.\n")
                     } else {
                       cat("FAILED check 3.\n")
                     }
                     
                   }
                   
                 })
                 
                 
                 
                 
               })
  
}
