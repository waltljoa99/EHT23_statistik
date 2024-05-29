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
  runAllChunks("ue4_angabe.Rmd")
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

# Function to remove outliers
remove_extreme_outliers <- function(data, cols, threshold = 1.5) {
  for (col in cols) {
    Q1 <- quantile(data[[col]], probs = .25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], probs = .75, na.rm = TRUE)
    IQR <- Q3 - Q1

    lower_bound <- Q1 - threshold * IQR
    upper_bound <- Q3 + threshold * IQR

    extreme_outliers <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
    data <- data[-extreme_outliers, ]
  }

  return(data)
}

handle_categorical_predictors_in_framingham <- function(data) {
  # Recode 'sex' and 'educ'
  data$sex <- dplyr::recode(data$sex, "men" = "1", "women" = "2")
  data$educ <- dplyr::recode(data$educ,
    "0-11 years" = "1",
    "High School/GED" = "2",
    "Some College/Vocational School" = "3",
    "College Degree (BSc/BA) or more" = "4"
  )
  data$cursmoke <- dplyr::recode(data$cursmoke, "no" = "0", "yes" = "1")
  data$bpmeds <- dplyr::recode(data$bpmeds, "no" = "0", "yes" = "1")
  data$diabetes <- dplyr::recode(data$diabetes, "no" = "0", "yes" = "1")

  # Convert variables to factors
  data$sex <- as.factor(data$sex)
  data$educ <- as.factor(data$educ)
  data$cursmoke <- as.factor(data$cursmoke)
  data$bpmeds <- as.factor(data$bpmeds)
  data$diabetes <- as.factor(data$diabetes)

  # Change the reference level of 'sex' to 0
  data$sex <- relevel(data$sex, ref = "1")

  # Change the reference level of 'educ' to 0
  data$educ <- relevel(data$educ, ref = "1")

  # Change the reference level of 'cursmoke' to 0
  data$cursmoke <- relevel(data$cursmoke, ref = "0")

  # Change the reference level of 'bpmeds' to 0
  data$bpmeds <- relevel(data$bpmeds, ref = "0")

  # Change the reference level of 'diabetes' to 0
  data$diabetes <- relevel(data$diabetes, ref = "0")

  return(data)
}
