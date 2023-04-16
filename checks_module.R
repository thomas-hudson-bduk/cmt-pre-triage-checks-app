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
        choices = c(
          "5. File format" = "option_5",
          "6. Column presence" = "option_6",
          "7. Naming conventions" = "option_7",
          "8. Additional info (colours and N/A)" = "option_8",
          "9. Are non standard columns logical" = "option_9",
          "10. UPRN corruption and truncation" = "option_10",
          "11. Missing UPRNs" = "option_11",
          "12. Duplicate UPRNs" = "option_12",
          "13. Blank rows" = "option_13"
        )
      ),
      conditionalPanel(
        condition = "input.selected_check == `option_12`",
        selectInput("column", label = "Select a column:", choices = NULL)
      )
    ),
    mainPanel(
      verbatimTextOutput(ns("upload_status")),
      verbatimTextOutput(ns("checks"))
    )
  )
}

# Define the server logic for a module
checks_server <- function(id, data, file_upload, session) {
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

      output$checks <- renderPrint({
        if (!is.null(file_upload()) & input$selected_check == "option_6") {
          source("check_6.R")
        }
        if (!is.null(file_upload()) & input$selected_check == "option_12") {
          source("check_12.R")
        }
      })

      #  output$check_3 <- renderPrint({
      #    if (!is.null(file_upload()) &
      #        input$selected_check == "3. Current Technology") {
      #      data = data()
      #      check_3_1 = 0
      #      check_3_2 = 0
      #      check_3_3 = 0

      #      if (all(is.numeric(data$current_max_download_speed), na.rm = TRUE)) {
      #        check_3_1 = 1
      #      }

      #      if (all(data$current_max_download_speed > 0, na.rm = TRUE)) {
      #        check_3_2 = 1
      #      }

      #      if (any(data$current_technology == "FTTP") &&
      #          all(data$current_max_download_speed[data$current_technology == "FTTP"] >= 1000, na.rm = TRUE)) {
      #        check_3_3 = 1
      #      }

      #      if (check_3_1 + check_3_2 + check_3_3 == 3) {
      #        cat("PASSED check 3.\n")
      #      } else {
      #        cat("FAILED check 3.\n")
      #      }

      #    }

      #  })
    }
  )
}
