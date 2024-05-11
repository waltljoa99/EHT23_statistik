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
  runAllChunks("ue3_angabe.Rmd")
}

########################################################################################
# Analysis of the Model
########################################################################################

load_and_prepare_framingham <- function(framingham) {
  # Read in data
  data <- read.spss(framingham, to.data.frame = TRUE)

  # Remove missing values
  data <- na.omit(data)

  # Handle Factor Variables
  data <- handle_categorical_predictors_in_framingham(data)

  return(data)
}

test_normality_of_groups <- function(data, group_var, var) {
  # Normal distribution in each group
  unique_groups <- unique(data[[group_var]])

  # Normal distribution
  par(mfrow = c(1, length(unique_groups)))
  for (group in unique_groups) {
    # Data for the specific group
    group_data <- data[[var]][data[[group_var]] == group]

    # Create histogram
    hist(group_data,
      probability = TRUE,
      main = paste("Histogram for", group_var, group),
      xlab = var,
      col = "gray", border = "black",
      ylim = c(0, 0.15)
    )

    # Add normal distribution curve
    curve(dnorm(x, mean = mean(group_data, na.rm = TRUE), sd = sd(group_data, na.rm = TRUE)), add = TRUE, col = "red")
  }

  # Shapiro Wilk test
  # Shapiro Wilk test
  for (group in unique_groups) {
    result <- shapiro.test(data[[var]][data[[group_var]] == group])
    result$data.name <- paste("data$", var, "[data$", group_var, "==", group, "]", sep = "")
    print(result)
  }
}

########################################################################################
# Helper functions for the exercises
########################################################################################


########################################################################################
# Visualisations
########################################################################################

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
