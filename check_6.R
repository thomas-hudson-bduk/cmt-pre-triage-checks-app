uploaded_data <- data()

# Check if all the required columns exist in the data table
missing_columns <-
    setdiff(tolower(required_columns), tolower(colnames(uploaded_data)))

# Output the result
if (length(missing_columns) == 0) {
    cat("PASSED check 6. \n\nAll required columns are present.\n")
} else {
    cat("FAILED check 6. \n\nThe following columns are missing:\n")
    cat(paste0("-", missing_columns, "\n"))
}
