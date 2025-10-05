
library(randomForest)
library(tidyverse)
library(caret)
library(ggplot2)
library(doParallel)
library(dplyr)

# Set base directory
baseDir <- "C:/Users/DELL/Desktop/Git_scudo/data/Benchmarking/noise_bench/run_5/10000_noise"
setwd(baseDir)

# Setup parallel backend
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# Initialize a data frame to collect R² scores
all_r2_results <- data.frame()

# Loop across noise levels using foreach
all_r2_results <- foreach(i = seq(0.0, 2.0, by = 0.5), .combine = rbind,
                          .packages = c("caret", "randomForest", "ggplot2",'tidyverse','dplyr')) %dopar% {
                            i_str <- formatC(i, format = "f", digits = 1)
                            cat("Processing run", i_str, "\n")
                            
                            run_name <- paste0("run_", i_str)
                            runfolder <- file.path(baseDir, run_name)
                            
                            train_name <- paste0("training_", i_str, "_dataset.csv")
                            valid_name <- paste0("validation_", i_str, "_dataset.csv")
                            
                            # Read data
                            training_data <- read.csv(file.path(baseDir, "datasets", train_name), row.names = 1)
                            validation_data <- read.csv(file.path(baseDir, "datasets", valid_name), row.names = 1)
                            training_metadata <- read.csv(file.path(baseDir, "datasets", "training_metadata.csv"), row.names = 1)
                            validation_metadata <- read.csv(file.path(baseDir, "datasets", "validation_metadata.csv"), row.names = 1)
                            
                            x_train <- t(training_data)
                            x_val <- t(validation_data)
                            y_train <- training_metadata$to_be_predicted
                            y_val <- validation_metadata$to_be_predicted
                            
                            train_df <- data.frame(y = y_train, x_train)
                            
                            rf_model <- train(
                              y ~ .,
                              data = train_df,
                              method = "rf",
                              ntree = 250,
                              importance = TRUE
                            )
                            var_imp <- varImp(rf_model)
                          
                            top50 <- var_imp$importance %>%
                              arrange(desc(Overall)) %>%
                              head(50)
                            
                            # Create output directory if it doesn't exist
                            if (!dir.exists(runfolder)) {
                              dir.create(runfolder, recursive = TRUE)
                            }
                            
                            write.csv(rownames(top50), file.path(runfolder, "feature_rf.csv"), row.names = FALSE)
                            
                            return(NULL)  # modify if you want to collect output
                          }

# Stop the cluster
stopCluster(cl)
