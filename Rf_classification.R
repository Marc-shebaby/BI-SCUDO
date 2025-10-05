# 0 packages
suppressPackageStartupMessages({
  library(randomForest)
  library(caret)   # only for confusionMatrix convenience
})
setwd('C:/Users/User/Desktop/scudo/scudo')
# 1 load
train <- read.csv("./100_trained/bi_scudo/expresssion_by_celltype_with_annotations.csv", header = TRUE, row.names = 1, check.names = FALSE)
valid <- read.csv("./100_trained/bi_scudo/validation.csv", header = TRUE, row.names = 1, check.names = FALSE)

# 2 extract labels from the last row BEFORE transpose
train_labels <- as.factor(as.character(unlist(train[nrow(train), ])))
valid_labels <- as.factor(as.character(unlist(valid[nrow(valid), ])))

# keep level order consistent
class_levels <- levels(train_labels)
valid_labels <- factor(valid_labels, levels = class_levels)

# 3 keep only expression rows then coerce to numeric and transpose
train_expr <- train[-nrow(train), , drop = FALSE]
valid_expr <- valid[-nrow(valid), , drop = FALSE]

train_data <- t(data.matrix(train_expr))
valid_data <- t(data.matrix(valid_expr))

stopifnot(nrow(train_data) == length(train_labels))
stopifnot(nrow(valid_data) == length(valid_labels))

# 4 fit default Random Forest
#set.seed(42)
rf_model <- randomForest(
  x = train_data,
  y = train_labels,
  ntree = 100 # default is 500, you can adjust
)

# 5 predict on validation set
pred_labels <- predict(rf_model, newdata = valid_data)

# 6 evaluate
cm <- confusionMatrix(pred_labels, valid_labels)
cat(sprintf("Validation accuracy: %.4f\n", cm$overall["Accuracy"]))
cat("Confusion matrix:\n")
print(cm$table)

# per class accuracy
per_class_acc <- diag(cm$table) / colSums(cm$table)
print(per_class_acc)

# 7 save confusion matrix
cm_RF_df <- as.data.frame.matrix(cm$table)
cm_RF_df_t <- as.data.frame(t(cm_RF_df))
write.csv(cm_RF_df_t, "CF_RF_100.csv", row.names = TRUE)

# 8 save per cell predictions
cell_ids <- rownames(valid_data)
out <- data.frame(
  cell = cell_ids,
  predicted = as.character(pred_labels),
  true = as.character(valid_labels),
  stringsAsFactors = FALSE
)
write.csv(out, "RF_100_per_cell_predictions.csv", row.names = FALSE)
