# Load packages and sources ----
source("packages.R")

version <- "0.4"
url <- a("Thomas Hudson (S&DI)", href = paste0("mailto:thomas.hudson@dcms.gov.uk?subject=CMT%20pre-triage%20application%20v", version, "%20-%20"))
info_text <- readr::read_file("lorum.txt")
required_columns <-
  as.vector(read.csv(
    "required_columns.csv",
    header = FALSE,
    colClasses = "character"
  )$V1)

# Define UI ----
ui <- fluidPage(
    tags$head(tags$style(
        HTML("pre { white-space: pre-wrap; word-break: keep-all; }")
    )),
    navbarPage(
        title = "CMT pre-triage application",
        tabPanel("Information", info_text),
        tabPanel("File Upload", sidebarLayout(
            sidebarPanel(
                fileInput(inputId = "file_upload", label = "Upload CSV File"),
                numericInput(
                    "size",
                    "Max File Size (MB)",
                    value = 500,
                    min = 0,
                    max = 10000,
                    step = 100
                ),
                actionButton("detailed_upload", "Detailed Output")
            ),
            mainPanel(
                verbatimTextOutput("file_info"),
                verbatimTextOutput("upload_output"),
                verbatimTextOutput("detailed_upload_output")
            ),
        )),
        tabPanel(
            "Data Summary",
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId = "view",
                        label = "",
                        choices = c(
                            "Head" = "head",
                            "Full data" = "full",
                            "Column summary" = "summary"
                        ),
                        selected = "head"
                    ),
                    conditionalPanel(
                        condition = "input.view == 'summary'",
                        selectInput("column_1", label = "Select a column:", choices = NULL),
                        radioButtons(
                            inputId = "show_unique",
                            label = "Show unique values:",
                            choices = c("Yes" = TRUE, "No" = FALSE),
                            selected = FALSE
                        )
                    ),
                    helpText(
                        "The 'Select a column' and input is only relevant for the 'Column summary' option."
                    )
                ),
                mainPanel(
                    verbatimTextOutput("upload_status_1"),
                    dataTableOutput("summary_data_table"),
                    verbatimTextOutput("column_summary"),
                    verbatimTextOutput("column_counts"),
                    dataTableOutput("unique_values"),
                )
            )
        ),
        tabPanel("Data Checks", sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "selected_check",
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
                    selectInput("column_2", label = "Select a column:", choices = NULL)
                )
            ),
            mainPanel(
                verbatimTextOutput("upload_status_2"),
                verbatimTextOutput("checks")
            )
        ))
    ),
    hr(),
    tagList("Version ", version, " - work in progress - ", url)
)


# Define server logic ----
server <- function(input, output, session) {
    observe({
        options(shiny.maxRequestSize = input$size * 1024^2)
    })

    output$file_info <- renderPrint({
        if (is.null(input$file_upload)) {
            cat("No file uploaded.")
        } else {
            cat(
                "File name:\t",
                input$file_upload$name,
                "\nFile type:\t",
                input$file_upload$type,
                "\nFile extension:\t",
                tools::file_ext(input$file_upload$name),
                "\nFile size:\t",
                round(input$file_upload$size / 1024^2, 1),
                " MB\n\n",
                sep = ""
            )
        }
    })

    upload_output <- reactive({
        req(input$file_upload)
        temp_file <- tempfile()
        sink(temp_file)
        fread(input$file_upload$datapath, verbose = TRUE)
        sink()
        output <- readLines(temp_file)
        file.remove(temp_file)
        output
    })

    data <- reactive({
        df <- fread(input$file_upload$datapath)
        as.data.frame(df)
    })

    file_upload <- reactive({
        input$file_upload
    })

    output$upload_output <- renderPrint({
        cat(gsub("wall clock time", "seconds.", upload_output()[grep("wall clock time", upload_output())[1]]),
            sep = "\n"
        )
    })

    output$detailed_upload_output <- renderPrint({
        if (input$detailed_upload %% 2 == 1) {
            cat(upload_output(), sep = "\n")
        }
    })

    observeEvent(file_upload(), {
        updateSelectInput(session, "column_1", choices = colnames(data()))
        updateSelectInput(session, "column_2", choices = colnames(data()))
    })


    output$upload_status_1 <- renderPrint({
        if (is.null(file_upload())) {
            cat("No file uploaded.")
        }
    })

    output$summary_data_table <- renderDataTable({
        if (!is.null(file_upload()) & input$view == "head") {
            head(data())
        } else if (!is.null(file_upload()) &
            input$view == "full") {
            data()
        }
    })

    output$column_summary <- renderPrint({
        if (!is.null(file_upload()) & input$view == "summary") {
            df <- data()
            df <- df[, input$column_1, drop = FALSE]
            # print(data)
            summary(df)
        }
    })

    output$column_counts <- renderPrint({
        if (!is.null(file_upload()) & input$view == "summary") {
            df <- data()
            df <- df[, input$column_1, drop = FALSE]

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
            as.data.frame(table(data()[, input$column_1, drop = FALSE]))
        }
    })

    output$upload_status_2 <- renderPrint({
        if (is.null(file_upload())) {
            cat("No file uploaded.")
        }
    })

    output$checks <- renderPrint({
        if (!is.null(file_upload()) & input$selected_check == "option_6") {
            source("check_6.R", local = TRUE)
        }
        if (!is.null(file_upload()) & input$selected_check == "option_12") {
            source("check_12.R", local = TRUE)
        }
                if (!is.null(file_upload()) & input$selected_check == "option_13") {
            source("check_13.R", local = TRUE)
        }
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
