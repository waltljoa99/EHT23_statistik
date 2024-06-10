########################################################################################
# For VSC User
########################################################################################

# Run all chunks in the document
# rmd ... name of document
runAllChunks <- function(rmd, envir = globalenv()) {
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  knitr::purl(rmd, output = tempR)
  sys.source(tempR, envir = envir)
  unlink(tempR)
}

# Run all chunks in THIS document
run <- function() {
  runAllChunks("ue5_angabe.Rmd")
}

########################################################################################
# Analysis of the Model
########################################################################################

# Function to create combined histogram plots
create_histograms <- function(data) {
  # Get the names of the numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  # Loop over the numeric columns
  for (col in numeric_cols) {
    # Create a histogram for the current column
    p <- ggplot(data, aes_string(x = col, fill = "group")) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
      labs(x = col, y = "Count", fill = "Group") +
      theme_minimal()

    # Print the histogram
    print(p)
  }
}

########################################################################################
# Helper functions for the exercises
########################################################################################
