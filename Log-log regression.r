# Function for log-log regression analysis for pixel-level data
# Returns sensitivity coefficients, p-values, model p-value, and R-squared

log_log_hua <- function(test) {
  # Required libraries
  library(tidyverse)
  
  # Helper function to calculate overall model p-value
  overall_p <- function(lm_result) {
    f <- summary(lm_result)$fstatistic
    p <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    attributes(p) <- NULL
    return(p)
  }
  
  # Define variable names (response and predictors)
  varnames <- c("var_y", "SOT")  # Response variable and predictors
  varnum <- length(varnames)
  
  # Create a data frame from input and remove rows with missing values
  test_df <- na.omit(as.data.frame(matrix(as.numeric(test), ncol = varnum, byrow = FALSE)))
  colnames(test_df) <- varnames
  
  # Define result names for output and prepare NA-filled results
  result_names <- c(
    paste0("sen_", varnames[-1]),   # Sensitivity coefficients
    paste0("pval_", varnames[-1]), # Predictor p-values
    "model_pvalue",                # Overall model p-value
    "model_r2"                     # R-squared of the model
  )
  NA_result <- rep(NA, length(result_names))
  names(NA_result) <- result_names
  
  # Ensure sufficient observations for reliable regression (at least 7 years)
  if (nrow(test_df) < 7) return(NA_result)
  
  # Check for constant variables by calculating the range of each column
  ranges <- apply(test_df, 2, function(col) max(col, na.rm = TRUE) - min(col, na.rm = TRUE))
  if (0 %in% ranges) return(NA_result) # Exclude pixels with constant variables
  
  # Apply log transformation (add small constant to avoid log(0))
  test_df <- log(test_df + 1e-8)
  
  # Perform linear regression
  mod <- lm(var_y ~ ., data = test_df)
  mod_summary <- summary(mod)
  
  # Extract sensitivity coefficients and p-values for predictors
  sen_vec <- round(mod_summary$coefficients[-1, 1], 3) %>% `names<-`(paste0("sen_", varnames[-1]))
  pval_vec <- round(mod_summary$coefficients[-1, 4], 3) %>% `names<-`(paste0("pval_", varnames[-1]))
  
  # Extract overall model p-value and R-squared
  mlr_pval <- round(overall_p(mod), 3) %>% `names<-`("model_pvalue")
  mlr_r2 <- round(mod_summary$r.squared, 3) %>% `names<-`("model_r2")
  
  # Handle cases where model p-value is NA
  if (is.na(mlr_pval)) return(NA_result)
  
  # Combine results into a single output vector
  result_all <- c(sen_vec, pval_vec, mlr_pval, mlr_r2)
  
  return(result_all) # Return all calculated results
}
