########################################################################################
# For VSC User
########################################################################################

# Run all chunks in the document
# rmd ... name of document
runAllChunks <- function(rmd, envir=globalenv()){
    tempR <- tempfile(tmpdir = ".",fileext = ".R")
    knitr::purl(rmd, output=tempR)
    sys.source(tempR, envir=envir)
    unlink(tempR)
  } 

# Run all chunks in THIS document
run <- function(){
  runAllChunks("ue2_angabe.Rmd")
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


run_analysis_of_full_model <- function(data) {
  
  # Create the logistic regression model
  glm <- glm(mi_fchd ~ sex + age + educ + cursmoke + cigpday +
               bpmeds + totchol + sysbp + diabp + bmi + diabetes + heartrte +
               glucose, data = data, family = binomial)
  summary(glm)


  print_odds_ratios(glm)

  print_glm_parameters(glm,data)


  # Check model assumptions

  # Numerical predictors
  variables <- c("age", "cigpday", "totchol", "sysbp", "diabp", "bmi", "heartrte", "glucose")
  logit_data <- get_logit_data(glm,data)
  
  plot_logit(logit_data,variables)

  # Check for multicollinearity
  vif(glm)



  return(glm)
}

print_odds_ratios <- function(glm) {

  # Get coefficients
  coefficients <- coef(glm)

  # Get odds ratios
  odds_ratios <- exp(coefficients)

  print(odds_ratios)
}

get_logit_data <- function(glm,data) {
  # Get logodds
  logodds <- predict(glm, newdata=data)

  # Add logodds to the data
  data$logodds <- logodds

  return(data)
}

# Improve full model by reducing variables while AIC decreases
backward_method <- function(glm){
  # Apply backward model selection
  reduced_model <- step(glm, direction = "backward")
  return(reduced_model)
}

########################################################################################
# Helper functions for the exercises
########################################################################################
# GLM Model Performance
########################################################################################

print_glm_parameters <- function(glm,data) {
  print(paste("R^2: ", calc_mcfadden(glm,data)))
  goodness_of_fit <- calc_goodness_of_fit(glm)
  print(paste("Goodness of fit: ", goodness_of_fit$p.value))
  print(paste("AUROC: ", calc_auroc(glm,data)))
  metrics <- calc_metrics(glm,data)
  print(paste("Sensitivity:", metrics$sensitivity))
  print(paste("Specificity:", metrics$specificity))
  # Calculate the correctly classified percentage
  print(paste("Correctly classified:", metrics$correctly_classified * 100, "%"))
  # Calculate the falsely classified percentage
  print(paste("Falsely classified:", metrics$falsely_classified * 100, "%"))
}

# Calculate McFadden R^2
calc_mcfadden <- function(glm,data) {
  # Fit a null model
  null_model <- glm(mi_fchd ~ 1, data = data, family = "binomial")

  # Calculate the likelihood for the full and null models
  ll_full <- logLik(glm)
  ll_null <- logLik(null_model)

  # Calculate McFadden R-squared
  mcfadden_r2 <- 1 - (ll_full / ll_null)
  
  return(mcfadden_r2)
}

# Calculate the goodness of fit
calc_goodness_of_fit <- function(glm){
  # Perform the Hosmer-Lemeshow test
  hoslem_result <- hoslem.test(glm$y, fitted(glm), g = 10) # g = default
  
  return(hoslem_result)
}

# Calculate the AUROC
calc_auroc <- function(glm,data){
  # Predict the probabilities
  probabilities <- predict(glm, newdata = data, type = "response")

  # Calculate the AUROC
  roc_obj <- suppressMessages(roc(response = data$mi_fchd, predictor = probabilities))
  auroc <- auc(roc_obj)
  
  return(auroc)
}

# Calculate the sensitivity, specificity, correctly classified and falsely classified
calc_metrics <- function(glm,data){
  # Predict the probabilities
  probabilities <- predict(glm, newdata = data, type = "response")

  # Convert predictions and actual values to factors
  predictions <- as.factor(ifelse(probabilities > 0.5, "yes", "no"))
  actuals <- factor(data$mi_fchd, levels = c("no", "yes"))

  # Calculate the confusion matrix
  cm <- caret::confusionMatrix(predictions, actuals)

  sensitivity <- cm$byClass["Sensitivity"]
  specificity <- cm$byClass["Specificity"]

  # Calculate the correctly classified percentage
  correctly_classified <- sum(diag(cm$table)) / sum(cm$table)

  # Calculate the falsely classified percentage
  falsely_classified <- 1 - correctly_classified

   # Return the results as a list
  return(list(sensitivity = sensitivity, 
              specificity = specificity, 
              correctly_classified = correctly_classified, 
              falsely_classified = falsely_classified))
}

########################################################################################
# Visualisations
########################################################################################

# Rule of thumb: values > 0.5 problematic
plot_cooks_distance <- function(glm) {
  # Calculate Cook's distances
  cooks_d <- cooks.distance(glm)

  # Plot Cook's distances
  plot(cooks_d, ylab="Cook's distance", main="Cook's distance for each observation")
  abline(h = 4/nrow(data), col="red")
}

# Rule of thumb: values > 3 problematic
plot_residuals <- function(glm,data) {
  # Calculate standardized residuals
  standardized_residuals <- rstandard(glm)

  # Assuming 'mi_fchd' is your dependent variable with "yes" and "no"
  colors <- ifelse(data$mi_fchd == "yes", "red", "blue")

  # Plot the standardized residuals with colors
  plot(standardized_residuals, col = colors, main = "Standardized Residuals", ylab = "Residuals", xlab = "Index")

  # Add a legend outside the plot area
  par(xpd=TRUE)
  legend("topleft", inset = -0.2, legend = c("Yes", "No"), fill = c("red", "blue"), title = "mi_fchd")
  par(xpd=FALSE)
}

# Check whether there is a linear relation between logit and each predictor variable
plot_logit <- function(logit_data, variables) {
  for (var in variables) {
    p <- ggplot(logit_data, aes(x = logit_data[[var]], y = logit_data[["logodds"]])) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
      labs(title = paste("Relationship between", var, "and logodds"),
           x = var,
           y = "Log(odds)")
    print(p)
  }
}

########################################################################################

# Function to remove extreme outliers
# Not used in the exercises
remove_extreme_outliers <- function(data, cols) {
  for (col in cols) {
    Q1 <- quantile(data[[col]], probs = .25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], probs = .75, na.rm = TRUE)
    IQR <- Q3 - Q1

    lower_bound <- Q1 - 3 * IQR
    upper_bound <- Q3 + 3 * IQR

    extreme_outliers <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
    data <- data[-extreme_outliers, ]
  }
  
  return(data)
}

handle_categorical_predictors_in_framingham <- function(data){
  # Recode 'sex' and 'educ'
  data$sex <- dplyr::recode(data$sex, "men" = "1", "women" = "2")
  data$educ <- dplyr::recode(data$educ, 
                    "0-11 years" = "1", 
                    "High School/GED" = "2", 
                    "Some College/Vocational School" = "3",
                    "College Degree (BSc/BA) or more" = "4")
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