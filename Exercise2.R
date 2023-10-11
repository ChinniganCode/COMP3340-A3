library("cccd")
library("igraph")
library("randomForest")
library("class")
library("infotheo")
library("mlbench")
library("caret")
red_dataset <- read.csv("datasets/winequality-red.csv", header = TRUE, sep = ";")
white_dataset <- read.csv("datasets/winequality-white.csv", header = TRUE, sep = ";")

#convert quality into a binary metric, quality <6 bad, everything else good
discretize_quality <- function(dataset) {
  dataset$quality <- ifelse(dataset$quality < 6, "bad", "good")
  return(dataset)
}

compute_metrics <- function(true_labels, predicted_labels) {

  cm <- table(true_labels, predicted_labels)
  TP <-  as.numeric(cm[2, 2])
  TN <-  as.numeric(cm[1, 1])
  FP <-  as.numeric(cm[1, 2])
  FN <-  as.numeric(cm[2, 1])

  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  precision <- TP / (TP + FP)
  f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  youden_j <- sensitivity + specificity - 1
  cat("Sensitivity: ", sensitivity, "\n")
  cat("Specificity: ", specificity, "\n")
  cat("Accuracy: ", accuracy, "\n")
  cat("F1: ", f1_score, "\n")
  cat("MCC: ", mcc, "\n")
  cat("Youden_J: ", youden_j, "\n")
}

go <- function(dataset) {
  data <- discretize_quality(dataset)
  data <- as.data.frame(data)
  target <- data$quality
  features <- data[, -which(names(data) == "quality")]

  cat("Length of target:", length(target), "\n")
  cat("Number of rows in features:", nrow(features), "\n")

  if (!is.factor(target)) {
    target <- as.factor(target)
  }

  rf_model <- randomForest(features, target)


  feature_importance <- importance(rf_model)
  print("--FEAT ROWS--")
  print(feature_importance)

  threshold <- quantile(feature_importance[, "MeanDecreaseGini"], 0.75)
  selected_rows <- feature_importance[, "MeanDecreaseGini"] > threshold
  top_features <- rownames(feature_importance)[selected_rows]
  cat("Number of top features:", length(top_features), "\n")
  print(top_features)
  selected_features <- features[, top_features]

  set.seed(123)
  index <- sample(1:nrow(selected_features), nrow(selected_features) * 0.8)
  train_features <- selected_features[index,]
  test_features <- selected_features[-index,]
  train_target <- target[index]
  test_target <- target[-index]

  predicted_labels <- knn(train_features, test_features, train_target, k = 5)
  print(predicted_labels)
  compute_metrics(test_target, predicted_labels)
}

go(red_dataset)
go(white_dataset)


