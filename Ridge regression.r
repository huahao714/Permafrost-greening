# Function to perform ridge regression analysis for pixel-level data
# and return sensitivity coefficients, p-values, model p-value, R-squared, and optimal ridge parameter (K).

ridge_hua5 <- function(test) {
  # Required libraries
  library(lmridge)
  library(tidyverse)
  
  # Define variable names (response and predictors)
  varnames <- c("var_y", "SOT", "CO2", "tmn", "ppt", "srad")
  varnum <- length(varnames)
  
  # Create a data frame from input and remove rows with missing values
  test_df <- na.omit(as.data.frame(matrix(as.numeric(test), ncol = varnum, byrow = FALSE)))
  colnames(test_df) <- varnames
  
  # Define result names for output and prepare NA-filled results
  result_names <- c(
    paste0("sen_", varnames[-1]),        # Sensitivity coefficients
    paste0("pval_", varnames[-1]),      # Predictor p-values
    "model_pvalue",                     # Overall model p-value
    "model_r2",                         # R-squared of the model
    "K_best"                            # Best ridge parameter
  )
  NA_result <- rep(NA, length(result_names))
  names(NA_result) <- result_names
  
  # Ensure sufficient observations for reliable regression (at least 7 years)
  if (nrow(test_df) < 7) return(NA_result)
  
  # Check for constant variables by calculating the range of each column
  ranges <- as.numeric(apply(test_df, 2, function(col) max(col, na.rm = TRUE) - min(col, na.rm = TRUE)))
  if (0 %in% ranges) return(NA_result) # Exclude pixels with constant variables
  
  # Set ridge regression parameter (K_best)
  # Alternatively, compute K_best dynamically if needed
  K_best <- optimize_k(test_df)  # Uncomment and define if dynamic optimization is required
  # K_best <- 2 # Fixed K_best value for simplicity
  
  # Perform ridge regression
  mod <- lmridge(var_y ~ ., data = test_df, K = K_best, scaling = "centered")
  mod_summary <- summary(mod)
  
  # Extract sensitivity coefficients and p-values for predictors
  summary_df <- mod_summary$summaries$`summary  1`
  sen_vec <- summary_df$coefficients[-1, 1] %>% `names<-`(paste0("sen_", varnames[-1]))
  pval_vec <- summary_df$coefficients[-1, 5] %>% `names<-`(paste0("pval_", varnames[-1]))
  
  # Extract overall model p-value and R-squared
  mlr_pval <- summary_df$fpvalue %>% `names<-`("model_pvalue")
  mlr_r2 <- summary_df$stats[, "R2"] %>% `names<-`("model_r2")
  
  # Combine results into a single output vector
  result_all <- c(sen_vec, pval_vec, mlr_pval, mlr_r2, "K_best" = K_best)
  
  return(result_all) # Return all calculated results
}

