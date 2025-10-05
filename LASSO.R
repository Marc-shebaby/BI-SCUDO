# Load libraries
library(glmnet)    # Lasso
library(tidyverse) # Data handling
library(caret)     # Preprocessing
# Set base directory
setwd("C:/Users/DELL/Desktop/Git_scudo/data/Benchmarking/noise_bench/run_5/10000_noise")
baseDir <- "C:/Users/DELL/Desktop/Git_scudo/data/Benchmarking/noise_bench/run_5/10000_noise"
#parent <- "C:/Users/DELL/Desktop/Git_scudo/data/Benchmarking/No_noise/150"

 #List all run folders (e.g., run_1, run_2, ...)
#run_folders <- list.dirs(baseDir, recursive = FALSE, full.names = TRUE) %>%
 # grep("run_\\f+$", ., value = TRUE)

# Initialize a data frame to collect R² scores
all_r2_results <- data.frame()
k <- 10 # Number of folds
# Loop through each run folder
#for (runfolder in run_folders) {
 #cat("Processing:", runfolder, "\n")
  #run_name <- basename(runfolder)
  
#k <- 10 # Number of folds

  for (i in seq(0, 2.0, by = 0.5)) {
    i_str <- formatC(i, format = "f", digits = 1)  # e.g., "0.5", "1.0"
    
    cat("Processing run", i_str, "\n")
    
    run_name <- paste0("run_", i_str)             # e.g., "run_0.5"
    runfolder <- file.path(baseDir, run_name)
    
    # Do something with runfolder
    print(runfolder)
  #training_data <- read.csv(file.path(baseDir, "datasets", paste0(run_name, "_sampled_training.csv")), row.names = 1)
  #training_metadata <- read.csv(file.path(baseDir,run_name,"datasets","metadata_training_shifted.csv"), row.names = 1)
  train_name <- paste0("training_", i_str, "_dataset.csv")
  valid_name <- paste0("validation_", i_str, "_dataset.csv")
  training_data <- read.csv(file.path(baseDir,"datasets",train_name), row.names = 1) 
  validation_data <- read.csv(file.path(baseDir,"datasets",valid_name), row.names = 1) 
  #validation_data <- read.csv(file.path(baseDir,run_name,"datasets","validation_dataset.csv"), row.names = 1)
  #validation_metadata <- read.csv(file.path(parent,run_name,"datasets","validation_metadata.csv"), row.names = 1)
  
  #validation_data <- read.csv(file.path(parent,run_name,"datasets","expression_data_for_validation.csv"), row.names = 1)
  validation_metadata <- read.csv(file.path(baseDir,"datasets","validation_metadata.csv"), row.names = 1)
# Load datasets
 # training_data <- read.csv(file.path(baseDir,run_name, "datasets", "training_dataset.csv"), row.names = 1)
#training_data <- read.csv(file.path(runfolder, "datasets", "expression_data_for_regression.csv"), row.names = 1)
#validation_data <- read.csv(file.path(parent, run_name,"datasets", "expression_data_for_validation.csv"), row.names = 1)
  training_metadata <- read.csv(file.path(baseDir,"datasets", "training_metadata.csv"), row.names = 1)
  #validation_metadata <- read.csv(file.path(parent,run_name ,"datasets", "validation_metadata.csv"), row.names = 1)
#validation_metadata <- read.csv(file.path(runfolder, "datasets", "validation_metadata (1).csv"), row.names = 1)



#validation_data <- read.csv(file.path(runfolder, "datasets", "validation_dataset.csv"), row.names = 1)
#training_metadata <- read.csv(file.path(runfolder, "datasets", "training_metadata.csv"), row.names = 1)
#validation_metadata <- read.csv(file.path(runfolder, "datasets", "validation_metadata.csv"), row.names = 1)

# Transpose expression data
x_train <- t(training_data)
x_val <- t(validation_data)

# Target variable (assumes column name is 'to_be_predicted')
y_train <- training_metadata$to_be_predicted
y_val <- validation_metadata$to_be_predicted

# Remove near-zero variance predictors
'if (length(nzv) > 0) {
  x_train <- x_train[, -nzv]
  x_val <- x_val[, -nzv]  # Important: apply same filtering to validation set
}'

# Convert to matrix (required by glmnet)
x_train <- as.matrix(x_train)
x_val <- as.matrix(x_val)

# Create custom fold IDs
create_custom_folds <- function(n_samples, k) {
  training_sets <- matrix(TRUE, nrow = k, ncol = n_samples)
  if (k > 1) {
    for (i in 1:k) {
      validation_index <- seq(i, n_samples, by = k)
      training_sets[i, validation_index] <- FALSE
    }
  }
  return(training_sets)
}

custom_folds <- create_custom_folds(nrow(x_train), k)

fold_ids <- rep(NA, nrow(x_train))
for (i in 1:k) {
  fold_ids[!custom_folds[i, ]] <- i
}

# Fit Lasso model with CV
set.seed(123)
cv_model <- cv.glmnet(
  x_train, y_train,
  alpha = 1,
  foldid = fold_ids,
  type.measure = "mse"
)
best_lambda <- coef(cv_model,s="lambda.min")
selected_features <- rownames(best_lambda)[which(best_lambda !=0)]
selected_features
write.csv(selected_features, file.path(baseDir,run_name, "feature_LASSO.csv"), row.names = FALSE)
#eport training R^2 (based on best lambda)
#y_pred_train <- predict(cv_model, newx = x_train, s = "lambda.min")
#r2_train <- 1 - sum((y_train - y_pred_train)^2) / sum((y_train - mean(y_train))^2)
#cat("Training R^2:", r2_train, "\n")

### R2 of CV
# Compute cross-validated MSE at best lambda
#cv_mse <- cv_model$cvm[cv_model$lambda == cv_model$lambda.min]

# Compute total variance of y_train
#total_var <- var(y_train)

# Compute R^2 from CV
#r2_cv <- 1 - (cv_mse / total_var)

#cat("Cross-validated R^2:", r2_cv, "\n")

# Predict and evaluate on validation data
#y_pred_val <- predict(cv_model, newx = x_val, s = "lambda.min")
#r2_val <- 1 - sum((y_val - y_pred_val)^2) / sum((y_val - mean(y_val))^2)
#cat("Validation R^2:", r2_val, "\n")
}

#plots
'library(ggplot2)

# Create data frame for training plot
df_train <- data.frame(
  Actual = y_train,
  Predicted = as.vector(y_pred_train)
)

# Plot
p_train <- ggplot(df_train, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Training: Actual vs Predicted",
       x = "Actual y",
       y = "Predicted y") #+
  #theme_minimal()
ggsave(paste0(run_name,"training_plot_LASSO.png"), plot = p_train, width = 6, height = 5, dpi = 300)

# Create data frame for validation plot
df_val <- data.frame(
  Actual = y_val,
  Predicted = as.vector(y_pred_val)
)

# Plot
p_val <- ggplot(df_val, aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Validation: Actual vs Predicted",
       x = "Actual y",
       y = "Predicted y") #+
  #theme_minimal()
ggsave(paste0(run_name,"Val_plot_LASSO.png"), plot = p_val, width = 6, height = 5, dpi = 300)

# Plot CV results
#plot(cv_model)
#ggsave("CV_LASSO.png", plot = p_train, width = 6, height = 5, dpi = 300)
#SAVE R2
# Create a data frame with the R^2 values
r2_results <- data.frame(
  Metric = c("Training R2", "Validation R2", "Cross-validated R2"),
  R2_Value = c(r2_train, r2_val, r2_cv)
)

# Write to CSV
#write.csv(r2_results, file.path(runfolder, "r2_scores_LASSO.csv"), row.names = FALSE)
write.csv(r2_results, file.path(baseDir,run_name, "r2_scores_LASSO.csv"), row.names = FALSE)
break
'


