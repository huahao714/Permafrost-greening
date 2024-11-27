# Load required libraries
library(caret)
library(ranger)
library(tidyverse)
library(treeshap)
library(parallel)

# Function to train Random Forest model with hyperparameter tuning
train_random_forest <- function(temp_df) {
  # Define the hyperparameter grid
  tuneGrid <- expand.grid(
    mtry = c(1:4),                       # Number of variables to try at each split
    splitrule = c("variance", "extratrees"), # Splitting rules
    min.node.size = c(1, 5, 10)          # Minimum size of terminal nodes
  )
  
  # Define cross-validation method
  control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
  
  # Train the Random Forest model
  model <- train(
    y ~ .,                               # Response variable and predictors
    data = temp_df,                      # Input dataset
    method = "ranger",                   # Random Forest implementation
    tuneGrid = tuneGrid,                 # Hyperparameter grid
    trControl = control,                 # Cross-validation control
    importance = "impurity"              # Variable importance metric
  )
  
  # Output the R² value of the final model
  cat("OOB R^2:", model$finalModel$r.squared, "\n")
  return(model)  # Return the trained model
}

# Function to compute SHAP values
compute_shap_values <- function(model, temp_df) {
  # Prepare the model for SHAP analysis
  model_unified <- ranger.unify(model$finalModel, temp_df)
  
  # Set up parallel computing
  cl <- makeCluster(detectCores() - 2)  # Use available cores minus 2
  clusterExport(cl, varlist = c("model_unified", "temp_df"))  # Export necessary objects to the cluster
  clusterEvalQ(cl, library(treeshap))  # Load required library on each node
  
  # Parallel SHAP value computation
  shap_results <- parLapply(cl, seq_len(nrow(temp_df)), function(i) {
    treeshap(model_unified, temp_df[i, , drop = FALSE])$shaps
  })
  stopCluster(cl)  # Stop the cluster
  
  # Combine SHAP results into a data frame
  shap_df <- do.call(rbind, shap_results) %>% as.data.frame()
  colnames(shap_df) <- colnames(temp_df)[-1]  # Use predictor names as column names
  return(shap_df)  # Return SHAP values
}

# Function to plot SHAP dependence
plot_shap_dependence <- function(shap_df, temp_df, variable_name) {
  # Check if the variable exists in the datasets
  if (!(variable_name %in% colnames(shap_df)) || !(variable_name %in% colnames(temp_df))) {
    stop(paste("Variable", variable_name, "not found in shap_df or temp_df."))
  }
  
  # Prepare the data for plotting
  plot_data <- data.frame(
    Variable = temp_df[[variable_name]],  # Predictor values
    SHAP_Value = shap_df[[variable_name]] # SHAP values
  )
  
  # Create SHAP dependence plot
  ggplot(plot_data, aes(x = Variable, y = SHAP_Value)) +
    geom_point(alpha = 0.2, color = "blue") +  # Scatter points
    geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +  # Regression line
    labs(
      title = paste("SHAP Dependence Plot for", variable_name),
      x = paste(variable_name, "(Input Variable)"),
      y = paste("SHAP Value for", variable_name)
    ) +
    theme_minimal(base_size = 15)  # Minimal theme for better visuals
}

# Example: Run the full analysis
# temp_df should be the dataset containing predictors and response variable (y)

# 1. Train the Random Forest model
model <- train_random_forest(temp_df)

# 2. Compute SHAP values
shap_df <- compute_shap_values(model, temp_df)

# 3. Visualize variable importance
df_vip <- data.frame(
  Variable = names(model$finalModel$variable.importance),
  Importance = round(model$finalModel$variable.importance, 10)
) %>% arrange(desc(Importance))

vip_plot <- ggplot(df_vip, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variable", y = "Importance") +
  theme_minimal(base_size = 15)
print(vip_plot)

# 4. SHAP dependence analysis
shap_plot <- plot_shap_dependence(shap_df, temp_df, "tmp_slope")  # Replace "tmp_slope" with the variable name of interest
print(shap_plot)
