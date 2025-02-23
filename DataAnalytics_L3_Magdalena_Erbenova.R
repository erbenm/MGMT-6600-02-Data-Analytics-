########
##LAB3##
########

# Load necessary libraries
library(readxl)
library(class)
library(caret)
library(ggplot2)
library(dplyr)
library(cluster)

# Read dataset
abalone_dataset <- read_excel("C:/Users/Magdalena Erbenova/Downloads/abalone_dataset.xlsx")

# Rename dataset
dataset <- abalone_dataset

# ==========================
# EXERCISE 1: k-NN
# ==========================

# Create age groups based on rings
dataset$age.group <- cut(dataset$rings, breaks = c(0, 8, 11, 35), labels = c("young", "adult", "old"))

# Split into training (80%) and testing (20%)
set.seed(42)
train_index <- createDataPartition(dataset$age.group, p = 0.8, list = FALSE)
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

# Define feature subsets (Balanced number of features)
features_1 <- c("length", "diameter", "rings")  
features_2 <- c("whole_weight", "shell_weight", "height")  

# Scale the features
train_scaled1 <- scale(train_data[, features_1])
test_scaled1 <- scale(test_data[, features_1])
train_scaled2 <- scale(train_data[, features_2])
test_scaled2 <- scale(test_data[, features_2])

# Train and evaluate k-NN Model 1 (k = 13)
knn_predicted1 <- knn(train = train_scaled1, test = test_scaled1, cl = train_data$age.group, k = 13)
conf_matrix1 <- table(Predicted = knn_predicted1, Actual = test_data$age.group)
accuracy1 <- sum(diag(conf_matrix1)) / nrow(test_data)

# Train and evaluate k-NN Model 2 (k = 13)
knn_predicted2 <- knn(train = train_scaled2, test = test_scaled2, cl = train_data$age.group, k = 13)
conf_matrix2 <- table(Predicted = knn_predicted2, Actual = test_data$age.group)
accuracy2 <- sum(diag(conf_matrix2)) / nrow(test_data)

# Print results
print("Confusion Matrix - Model 1")
print(conf_matrix1)
print(paste("Accuracy of Model 1:", round(accuracy1 * 100, 2), "%"))

print("Confusion Matrix - Model 2")
print(conf_matrix2)
print(paste("Accuracy of Model 2:", round(accuracy2 * 100, 2), "%"))

# Determine best model
if (accuracy1 > accuracy2) {
  print("Model 1 performed better.")
  best_features <- features_1
  best_train <- train_scaled1
  best_test <- test_scaled1
} else {
  print("Model 2 performed better.")
  best_features <- features_2
  best_train <- train_scaled2
  best_test <- test_scaled2
}

# Find optimal k
k.list <- seq(1, 71, by = 2)  # Smaller range for efficiency
accuracy.list1 <- c()
accuracy.list2 <- c()

for (k in k.list) {
  # Model 1
  knn_predicted1 <- knn(train = train_scaled1, test = test_scaled1, cl = train_data$age.group, k = k)
  accuracy.list1 <- c(accuracy.list1, sum(diag(table(knn_predicted1, test_data$age.group))) / length(test_data$age.group))
  
  # Model 2
  knn_predicted2 <- knn(train = train_scaled2, test = test_scaled2, cl = train_data$age.group, k = k)
  accuracy.list2 <- c(accuracy.list2, sum(diag(table(knn_predicted2, test_data$age.group))) / length(test_data$age.group))
}

# Plot accuracy vs. k-value
ggplot(data.frame(k = rep(k.list, 2), accuracy = c(accuracy.list1, accuracy.list2), 
                  model = rep(c("Model 1", "Model 2"), each = length(k.list))),
       aes(x = k, y = accuracy, color = model)) +
  geom_line() + geom_point() +
  labs(title = "K-NN Accuracy vs. K-Value", x = "Number of Neighbors (k)", y = "Accuracy")

# ==========================
# EXERCISE 2: K-Means
# ==========================

# Scale the best-performing feature subset
scaled_features <- scale(dataset[, best_features])

# Run initial K-Means (k = 5)
k <- 5
dataset.km <- kmeans(scaled_features, centers = k, nstart = 10)
dataset$assigned.clusters <- as.factor(dataset.km$cluster)

# Plot initial clustering
ggplot(dataset, aes_string(x = best_features[1], y = best_features[2], color = "assigned.clusters")) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = paste("Initial K-Means Clustering (K =", k, ")"),
       x = best_features[1], y = best_features[2]) 

# Compute WCSS for multiple k-values
k.list <- seq(2, 20, by = 2)
wcss.list <- c()

for (k in k.list) {
  dataset.km <- kmeans(scaled_features, centers = k, nstart = 10)
  wcss.list <- c(wcss.list, dataset.km$tot.withinss)
}

# Plot WCSS vs. k
plot(k.list, wcss.list, type = "b", pch = 19, col = "pink",
     xlab = "Number of Clusters", ylab = "WCSS",
     main = "Elbow Method for Optimal K")

# Find optimal k (better method)
optimal_k <- k.list[which.min(diff(diff(wcss.list))) + 1]

# Run final K-Means with optimal k
dataset.km <- kmeans(scaled_features, centers = optimal_k, nstart = 10)
dataset$assigned.clusters <- as.factor(dataset.km$cluster)

# Plot final clustering
ggplot(dataset, aes_string(x = best_features[1], y = best_features[2], color = "assigned.clusters")) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = paste("Final K-Means Clustering (Optimal K =", optimal_k, ")"),
       x = best_features[1], y = best_features[2])

##based on the previous model the optimal number of clusters is 16  