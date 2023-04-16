# Define the UI for a module
summary_ui <- function(id, label = "summary_ui") {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = ns("view"),
        label = "",
        choices = c(
          "Head" = "head",
          "Full data" = "full",
          "Column summary" = "summary"
        ),
        selected = "head"
      ),
      selectInput(ns("column"), label = "Select a column:", choices = NULL),
      radioButtons(
        inputId = ns("show_unique"),
        label = "Show unique values:",
        choices = c("Yes" = TRUE, "No" = FALSE),
        selected = FALSE
      ),
      helpText(
        "The 'Select a column' and input is only relevant for the 'Column summary' option."
      )
    ),
    mainPanel(
      verbatimTextOutput(ns("upload_status")),
      dataTableOutput(ns("summary_data_table")),
      verbatimTextOutput(ns("column_summary")),
      verbatimTextOutput(ns("column_counts")),
      dataTableOutput(ns("unique_values")),
    )
  )
}

# Define the server logic for a module
summary_server <- function(id, data, file_upload) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(file_upload(), {
        updateSelectInput(session, "column", choices = colnames(data))
      })


      output$upload_status <- renderPrint({
        if (is.null(file_upload())) {
          cat("No file uploaded.")
        }
      })

      output$summary_data_table <- renderDataTable({
        if (!is.null(file_upload()) & input$view == "head") {
          head(data)
        } else if (!is.null(file_upload()) &
          input$view == "full") {
          data
        }
      })

      output$column_summary <- renderPrint({
        if (!is.null(file_upload()) & input$view == "summary") {
          df <- data
          df <- df[, input$column, drop = FALSE]
          # print(data)
          summary(df)
        }
      })

      output$column_counts <- renderPrint({
        if (!is.null(file_upload()) & input$view == "summary") {
          df <- data
          df <- df[, input$column, drop = FALSE]

          result <- as.data.frame(t(
            list(
              Row_count = nrow(df),
              NULL_count = sum(is.na(df)),
              Unique_count = n_distinct(na.omit(df)),
              Duplicate_count = sum(duplicated(na.omit(df)))
            )
          ))
          result_df <- as.data.frame(t(result))
          colnames(result_df) <- "Counts"
          result_df <- result_df[, 1, drop = FALSE]

          result_df
        }
      })


      output$unique_values <- renderDataTable(rownames = FALSE, {
        if (!is.null(file_upload()) && input$view == "summary" && as.logical(input$show_unique)) {
          as.data.frame(table(data[, input$column, drop = FALSE]))
        }
      })
    }
  )
}
