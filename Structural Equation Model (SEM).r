# Structural Equation Model (SEM) Function
sem_fun_hua <- function(test, nyear = 40) {
  # Load necessary libraries
  library(dplyr)
  library(lavaan)
  
  # Define variable names
  varnames <- c("VI", "SOT", "albedo", "GUD", "SMdif", "runoff_dif")
  varnum <- length(varnames)
  
  # Preprocess input data
  test_df <- na.omit(as.data.frame(matrix(as.numeric(test), ncol = varnum, byrow = FALSE)))
  colnames(test_df) <- varnames
  
  # Initialize result vector with NA values
  result_names <- c("gfi", "srmr", "rmsea", "cfi", "chisq", "df", "pvalue",
                    "A_std.all", "A_pvalue", "B_std.all", "B_pvalue",
                    "C_std.all", "C_pvalue", "D_std.all", "D_pvalue",
                    "E_std.all", "E_pvalue", "F_std.all", "F_pvalue",
                    "G_std.all", "G_pvalue", "H_std.all", "H_pvalue",
                    "I_std.all", "I_pvalue", "J_std.all", "J_pvalue",
                    "K_std.all", "K_pvalue", "R2_VI", "R2_albedo", 
                    "R2_GUD", "R2_SMdif", "R2_runoff_dif")
  NA_result <- rep(NA, length(result_names))
  names(NA_result) <- result_names
  
  # Check if data is sufficient for SEM analysis
  if (nrow(test_df) < floor(0.7 * nyear)) return(NA_result)
  
  # Standardize the data
  sem_df <- as.data.frame(scale(test_df))
  
  # Define SEM model
  model <- "
    VI ~ albedo + GUD + SMdif + SOT
    albedo ~ SOT
    GUD ~ albedo + SOT + SMdif
    SMdif ~ SOT + runoff_dif
    runoff_dif ~ SOT
  "
  
  # Fit SEM model
  fit <- tryCatch({
    lavaan::sem(model, data = sem_df)
  }, error = function(e) return(NA_result))
  
  # Extract fit measures
  fit_measures <- lavaan::fitMeasures(fit, c("gfi", "srmr", "rmsea", "cfi", "chisq", "df", "pvalue"))
  fit_measures <- round(fit_measures, 4)
  
  # Extract path coefficients and p-values
  params <- lavaan::parameterEstimates(fit, standardized = TRUE, rsquare = TRUE)
  extract_path <- function(lhs, rhs) {
    path <- params %>%
      filter(lhs == lhs, rhs == rhs, op == "~") %>%
      select(std.all, pvalue) %>%
      round(4)
    if (nrow(path) == 0) return(c(std.all = NA, pvalue = NA))
    return(c(std.all = path$std.all, pvalue = path$pvalue))
  }
  
  paths <- c(
    extract_path("albedo", "SOT"),
    extract_path("GUD", "SOT"),
    extract_path("SMdif", "SOT"),
    extract_path("runoff_dif", "SOT"),
    extract_path("GUD", "albedo"),
    extract_path("GUD", "SMdif"),
    extract_path("SMdif", "runoff_dif"),
    extract_path("VI", "albedo"),
    extract_path("VI", "GUD"),
    extract_path("VI", "SMdif"),
    extract_path("VI", "SOT")
  )
  names(paths) <- c("A_std.all", "A_pvalue", "B_std.all", "B_pvalue",
                    "C_std.all", "C_pvalue", "D_std.all", "D_pvalue",
                    "E_std.all", "E_pvalue", "F_std.all", "F_pvalue",
                    "G_std.all", "G_pvalue", "H_std.all", "H_pvalue",
                    "I_std.all", "I_pvalue", "J_std.all", "J_pvalue",
                    "K_std.all", "K_pvalue")
  
  # Extract R-squared values
  r2_values <- params %>%
    filter(op == "r2") %>%
    select(lhs, est) %>%
    mutate(est = round(est, 4)) %>%
    deframe()
  names(r2_values) <- paste0("R2_", names(r2_values))
  
  # Combine all results
  result <- c(fit_measures, paths, r2_values)
  result <- c(result, rep(NA, length(NA_result) - length(result)))  # Pad with NA if necessary
  names(result) <- result_names
  
  return(result)
}
