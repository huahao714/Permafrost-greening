# Function for causal analysis using CCM (Convergent Cross Mapping)
ccm_hua <- function(test, E0 = 3, sample0 = 30) {
  # Load required library
  library(rEDM)
  
  # Define function to generate libSizes for CCM
  generateLibSizes <- function(totalPoints, E = E0) {
    if (totalPoints <= E) {
      stop("Total points must be greater than E (Embedding Dimension).")
    }
    
    startSize <- E + 1                 # Minimum library size
    endSize <- totalPoints - 2         # Maximum library size
    interval <- max(2, floor((endSize - startSize) / 10)) # Interval between library sizes
    return(paste(startSize, endSize, interval, sep = " "))
  }
  
  # Input variable names
  varnames <- c("y", "x")              # 'y' is the target variable, 'x' is the driver
  varnum <- length(varnames)
  
  # Preprocess input data
  test_df <- na.omit(as.data.frame(matrix(as.numeric(test), ncol = varnum, byrow = F)))
  colnames(test_df) <- varnames
  
  # Define result names and initialize result vector
  result_names <- c("cor_r_y2x", "cor_p_y2x", "rho_mean_y2x", "rho_last_y2x",
                    "cor_r_x2y", "cor_p_x2y", "rho_mean_x2y", "rho_last_x2y")
  nresult <- length(result_names)
  
  # Check for sufficient observations
  min_points <- (E0 + 1) + 2 * (5 - 1) + 2  # Minimum points needed for CCM
  if (nrow(test_df[complete.cases(test_df),]) < min_points) {
    result <- rep(NA, nresult)
    names(result) <- result_names
    return(result)
  }
  
  # Add a time column for CCM
  test_df <- cbind(time = seq_len(nrow(test_df)), test_df)
  
  # Perform CCM
  ccm_df <- CCM(dataFrame = test_df, 
                E = E0, 
                Tp = 0, 
                columns = "x", 
                target = "y", 
                libSizes = generateLibSizes(nrow(test_df)), 
                sample = sample0, 
                showPlot = FALSE)
  
  # Correlation analysis for convergence
  cor_result_y2x <- cor.test(ccm_df$`y:x`, ccm_df$LibSize, method = "spearman")
  cor_result_x2y <- cor.test(ccm_df$`x:y`, ccm_df$LibSize, method = "spearman")
  
  # Handle missing results
  if (any(is.na(cor_result_y2x))) {
    result <- rep(NA, nresult)
    names(result) <- result_names
    return(result)
  }
  
  # Extract results for both directions
  result_y2x <- c(
    cor_result_y2x$estimate,           # Spearman correlation (x -> y)
    cor_result_y2x$p.value,           # p-value (x -> y)
    mean(ccm_df$`y:x`, na.rm = TRUE), # Mean predictive skill (x -> y)
    ccm_df$`y:x`[nrow(ccm_df)]        # Last predictive skill value (x -> y)
  )
  
  result_x2y <- c(
    cor_result_x2y$estimate,           # Spearman correlation (y -> x)
    cor_result_x2y$p.value,           # p-value (y -> x)
    mean(ccm_df$`x:y`, na.rm = TRUE), # Mean predictive skill (y -> x)
    ccm_df$`x:y`[nrow(ccm_df)]        # Last predictive skill value (y -> x)
  )
  
  # Combine results
  result <- c(result_y2x, result_x2y)
  names(result) <- result_names
  
  # Final check to ensure result integrity
  if (length(result) != nresult) {
    result <- rep(NA, nresult)
    names(result) <- result_names
  }
  
  return(result) # Return final results
}

