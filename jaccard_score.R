library(dplyr)
library(stringr)
library(ggplot2)
library(purrr)

# 1. Jaccard function
jaccard_index <- function(a, b) length(intersect(a, b)) / length(union(a, b))
true_feats <- paste0("feature_", 0:49)

# 2. Read SCUDO biomarker file
read_scudo_biomarker <- function(path) {
  lines <- readLines(path)
  data_lines <- grep("^feature_", lines, value = TRUE)
  df <- read.table(text = data_lines, header = FALSE, stringsAsFactors = FALSE)
  
  if (ncol(df) == 4) {
    colnames(df) <- c("Feature.ID", "p.value", "weight", "type")
  } else if (ncol(df) == 3) {
    colnames(df) <- c("Feature.ID", "p.value", "weight")
  } else {
    stop("Unexpected number of columns in biomarker file: ", ncol(df))
  }
  
  df$Feature.ID
}


# 3. Gather Jaccard scores
base_path <- "C:/Users/DELL/Desktop/Git_scudo/data/Benchmarking/noise_bench"
run_dirs <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)

all_scores <- tibble(
  dataset_size = character(),
  run_folder   = character(),
  jaccard      = numeric(),
  type         = character()
)

for (run_dir in run_dirs) {
  noise_dirs <- list.dirs(run_dir, recursive = FALSE, full.names = TRUE)
  for (noise_dir in noise_dirs) {
    size_label <- basename(noise_dir)  # e.g., "1000_noise"
    dataset_prefix <- str_remove(size_label, "_noise")  # e.g., "1000"
    
    run_folders <- list.dirs(noise_dir, recursive = FALSE, full.names = TRUE) %>%
      keep(~ str_starts(basename(.x), "run_"))
    
    for (run_folder in run_folders) {
      run_name <- basename(run_folder)
      
      # SCUDO
      biomarker_path <- file.path(run_folder, "_biomarker.txt")
      if (file.exists(biomarker_path)) {
        sel_feats   <- read_scudo_biomarker(biomarker_path)
        jaccard_val <- jaccard_index(sel_feats, true_feats)
        all_scores <- add_row(all_scores,
                              dataset_size = dataset_prefix,
                              run_folder   = run_name,
                              jaccard      = jaccard_val,
                              type         = "scudo"
        )
      }
      
      # LASSO
      lasso_path <- file.path(run_folder, "feature_LASSO.csv")
      if (file.exists(lasso_path)) {
        lasso_feats <- read.csv(lasso_path, stringsAsFactors = FALSE)$x
        lasso_feats <- lasso_feats[lasso_feats != "(Intercept)"]
        jaccard_lasso <- jaccard_index(lasso_feats, true_feats)
        all_scores <- add_row(all_scores,
                              dataset_size = dataset_prefix,
                              run_folder   = run_name,
                              jaccard      = jaccard_lasso,
                              type         = "lasso"
        )
      }
      
      # Random Forest
      rf_path <- file.path(run_folder, "feature_rf.csv")
      if (file.exists(rf_path)) {
        rf_feats <- read.csv(rf_path, stringsAsFactors = FALSE)$x
        jaccard_rf <- jaccard_index(rf_feats, true_feats)
        all_scores <- add_row(all_scores,
                              dataset_size = dataset_prefix,
                              run_folder   = run_name,
                              jaccard      = jaccard_rf,
                              type         = "rf"
        )
      }
    }
  }
}

# 4. Extract numeric noise level
all_scores <- all_scores %>%
  mutate(noise_increment = as.numeric(str_remove(run_folder, "^run_")))

# 5. Save each dataset plot separately
unique_datasets <- unique(all_scores$dataset_size)

for (ds in unique_datasets) {
  df_sub <- filter(all_scores, dataset_size == ds)
  
  p <- ggplot(df_sub, aes(x = factor(noise_increment), y = jaccard, fill = type)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
    labs(
      title = paste("Jaccard vs Noise Increment — Dataset size", ds),
      x     = "Noise Increment",
      y     = "Jaccard Similarity"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(
    filename = paste0("jaccard_dataset_", ds, ".png"),
    plot     = p,
    width    = 6,
    height   = 4,
    dpi      = 300
  )
}
