library(shiny)

# Define UI ----

info_text = readr::read_file("lorum.txt")
source("checks.R")

ui <- fluidPage(
    tags$head(tags$style(
        HTML("pre { white-space: pre-wrap; word-break: keep-all; }")
    )),
    
    navbarPage(
        title = "WIP: CMT pre-triage application - v0.2.0",
        
        tabPanel("Information", info_text),
        
        tabPanel("File Upload", sidebarLayout(
            sidebarPanel(
                fileInput(inputId = "file_upload", label = "Upload CSV File"),
                numericInput(
                    "size",
                    "Max File Size (MB)",
                    value = 1000,
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
            )
        )),
        
        tabPanel("Data Checks",  checks_ui("checks_module", "Checks UI"))
    )
)


# Define server logic ----
server <- function(input, output) {
    observe({
        options(shiny.maxRequestSize = input$size * 1024 ^ 2)
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
                round(input$file_upload$size / 1024 ^ 2, 1),
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
        req(input$file_upload)
        fread(input$file_upload$datapath)
    })
    
    checks_server("checks_module", data = data, file_upload = reactive(input$file_upload))
    
    output$upload_output <- renderPrint({
        cat(gsub("wall clock time", "seconds.", upload_output()[grep("wall clock time", upload_output())[1]]), sep = "\n")
    })
    
    output$detailed_upload_output <- renderPrint({
        if (input$detailed_upload %% 2 == 1) {
            cat(upload_output(), sep = "\n")
        }
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
