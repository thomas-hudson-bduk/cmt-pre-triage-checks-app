n_blank_rows <- sum(apply(data(), 1, function(row) all(is.na(row) | row == "")))

# Output the result
if (n_blank_rows == 0) {
    cat("PASSED check 13. \n\nNo blank rows are present.\n")
} else {
    cat(
        "FAILED check 13. \n\nThere are: ",
        format(n_blank_rows, big.mark = ","),
        " blank rows within the uploaded data.\n",
        sep = ""
    )
}