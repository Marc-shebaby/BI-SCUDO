library(dplyr)
library(stringr)
# Define paths
base_dir_150 <- "C:/Users/DELL/Desktop/Git_scudo/data/Benchmarking/150"
base_dir_100 <- "C:/Users/DELL/Desktop/Git_scudo/data/Benchmarking/100"
base_dir_50<- "C:/Users/DELL/Desktop/Git_scudo/data/Benchmarking/50"
df_list <- list()  # Initialize empty list

for (j in seq(50, 150, by = 50)) {
  df_list[[paste0("df_", j)]] <- data.frame(
    r_train = numeric(0),
    r_valid = numeric(0)
  )
}
# Folder containing run_1, run_2, etc.
#output_dir <- "C:/Users/DELL/Desktop/Git_scudo/data/Benchmarking/50"  # Output directory

# Create output directory
#if (!dir.exists(output_dir)){
 # print("false")
#} 
# List all run folders (e.g., run_1, run_2, ...)
run_folders_150 <- list.dirs(base_dir_150, recursive = FALSE, full.names = TRUE) %>%
  grep("run_\\d+", ., value = TRUE)  # Matches folders like "run_1", "run_2"
run_folders_100 <- list.dirs(base_dir_100, recursive = FALSE, full.names = TRUE) %>%
  grep("run_\\d+", ., value = TRUE)  # Matches folders like "run_1", "run_2"
run_folders_50 <- list.dirs(base_dir_50, recursive = FALSE, full.names = TRUE) %>%
  grep("run_\\d+", ., value = TRUE)  # Matches folders like "run_1", "run_2"

r_150 <- data.frame(training_r = numeric(0),valid_r=numeric(0))  # Initialize empty vector
r_100<- data.frame(training_r = numeric(0),valid_r=numeric(0))  # Initialize empty vector
r_50<- data.frame(training_r = numeric(0),valid_r=numeric(0))  # Initialize empty vector
i=150
while (i >0){
  folder_name <- get(paste0("run_folders_", i))
 
for (folder in folder_name) {
  # Paths to input files
  training_file <- file.path(folder, "_trainingDataset_results.txt")
  validate_file <- file.path(folder, "_validationDataset_results.txt")
  
  # Skip if files are missing
  if (!file.exists(training_file)) {
    warning("Skipping ", folder, ": training_dataset.csv not found")
    next
  }
  if (!file.exists(validate_file)) {
    warning("Skipping ", folder, ": metadata_training.csv not found")
    next
  
  }
  ##READ
  file_train<- readLines(training_file)
  file_validate<- readLines(validate_file)
  
  # Get the last line
  last_line_train <- tail(file_train, 1)
  last_line_validate <- tail(file_validate, 1)
  
  # Extract the numeric value using regular expression
  r_squared_train <- as.numeric(str_extract(last_line_train, "-?[0-9.]+"))
  r_squared_validate <- as.numeric(str_extract(last_line_validate, "-?[0-9.]+"))
  
 
  
  df_list[[paste0("df_", i)]][nrow(df_list[[paste0("df_", i)]]) + 1, "r_train"] <- r_squared_train
  df_list[[paste0("df_", i)]][nrow(df_list[[paste0("df_", i)]]), "r_valid"] <- r_squared_validate
  
 
}
   
  i=i-50
}
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Prepare the data (combine all dataframes with their names)
combined_data <- bind_rows(
  lapply(names(df_list), function(df_name) {
    df_list[[df_name]] %>% 
      mutate(source_df = df_name) %>%  # Add dataframe name as column
      pivot_longer(cols = -source_df)  # Convert to long format
  })
)

# 2. Create boxplot with dataframe names on x-axis
ggplot(combined_data, aes(x = source_df, y = value, fill = name)) +
  geom_boxplot() +
  labs(title = "Distribution by Dataframe and Column",
       x = "Dataframe Name",
       y = "R_squared",
       fill = "Column") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  