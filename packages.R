# Define the list of packages to load
packages <- c("shiny", "DT", "dplyr", "data.table", "readr")

# Loop through the packages and install/load them one at a time
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}