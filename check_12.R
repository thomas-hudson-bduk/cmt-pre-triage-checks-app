uprns <- data()[, 1]
duplicate_count <- sum(duplicated(na.omit(uprns)))

# Output the result
if (duplicate_count == 0) {
    cat("PASSED check 12. \n\nNo duplicate UPRNs are present.\n")
} else {
    cat(
        "FAILED check 12. \n\nThere are: ",
        format(duplicate_count, big.mark = ","),
        " duplicates within the '",
        colnames(uploaded_data)[[1]],
        "' column of the uploaded data.\n",
        sep = ""
    )
}
