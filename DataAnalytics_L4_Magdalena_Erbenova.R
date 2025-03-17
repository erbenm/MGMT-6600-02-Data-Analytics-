#importing the data
colnames(wine) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavinoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
head(wine)

#install packages
install.packages("caret")
install.packages("class")
install.packages("ggfortify")

#ready libraries
library(caret)
library(class)
library(ggfortify)
library(ggplot2)

#ensuring the column names are correct
colnames(wine) <- make.names(colnames(wine))
colnames(wine)[1] <- "Class"  #seting the first column name 

#preparing dataset
wine.df <- wine
X <- wine.df[, 2:14]
Y <- as.factor(wine.df$Class)  

#renaming columns 
colnames(wine.df) <- gsub(" ", "_", colnames(wine.df))

#scatter plot of Alcohol vs Malic Acid, colored by class
ggplot(wine.df, aes(x = Alcohol, y = Malic.acid, color = as.factor(Class))) + 
  geom_point() + 
  stat_ellipse(type = "t", geom = "polygon", alpha = 0.4) +
  labs(title = "Scatter plot of Alcohol vs Malic Acid", x = "Alcohol", y = "Malic Acid")

####### PCA #######

principal_components <- princomp(X, cor = TRUE, scores = TRUE)
summary(principal_components)

#identifying top contributing variables to PC1
loading_scores <- abs(principal_components$loadings[, 1])
top_contributors <- sort(loading_scores, decreasing = TRUE)[1:5]
print("Top contributing variables to PC1:")
print(top_contributors)

#plotting first two principal components
autoplot(principal_components, data = wine.df, colour = 'Class',
         x = 1, y = 2, loadings = TRUE, loadings.colour = 'yellow',
         loadings.label = TRUE, loadings.label.size = 3) +
  labs(title = "PCA - First Two Principal Components", x = "PC1", y = "PC2")

#identifying lowest contributing variables and removing them
low_contributors <- sort(loading_scores, decreasing = FALSE)[1:2]
filtered_wine <- wine[, !(names(wine) %in% names(low_contributors))]

#performing PCA on filtered dataset
X_filtered <- filtered_wine[, 2:ncol(filtered_wine)]
principal_components_filtered <- princomp(X_filtered, cor = TRUE, scores = TRUE)
summary(principal_components_filtered)

####### KNN on Original Dataset #######

set.seed(42) #ensure reproducibility
trainIndex <- createDataPartition(wine$Class, p = 0.8, list = FALSE)
wine.train <- wine[trainIndex, ]
wine.test <- wine[-trainIndex, ]

k <- round(sqrt(nrow(wine.train))) - 1

knn.predicted <- knn(train = wine.train[, 2:14], test = wine.test[, 2:14], 
                     cl = wine.train$Class, k = k)  #fixing reference to Class

contingency.table1 <- table(knn.predicted, wine.test$Class, dnn = list('Predicted', 'Actual'))
print(contingency.table1)

#calculating accuracy, precision, recall, F1-score
cm <- as.matrix(contingency.table1)
n <- sum(cm)
diag_values <- diag(cm)
rowsums <- apply(cm, 1, sum)
colsums <- apply(cm, 2, sum)

accuracy <- sum(diag_values) / n
precision <- diag_values / colsums
recall <- diag_values / rowsums
f1 <- 2 * precision * recall / (precision + recall)

print("Classification Metrics on Original Dataset:")
print(data.frame(Recall = recall, Precision = precision, F1 = f1))

####### KNN on PCA Scores #######

pca_scores <- as.data.frame(principal_components$scores[, 1:3])
pca_scores$Class <- wine$Class

trainIndex <- createDataPartition(pca_scores$Class, p = 0.8, list = FALSE)
pca.train <- pca_scores[trainIndex, ]
pca.test <- pca_scores[-trainIndex, ]

knn.predicted_pca <- knn(train = pca.train[, 1:3], test = pca.test[, 1:3], 
                         cl = pca.train$Class, k = k)  #fixing reference to Class

contingency.table_pca <- table(knn.predicted_pca, pca.test$Class, dnn = list('Predicted', 'Actual'))
print(contingency.table_pca)

#calculating accuracy, precision, recall, F1-score for PCA-based KNN
cm_pca <- as.matrix(contingency.table_pca)
diag_values_pca <- diag(cm_pca)
rowsums_pca <- apply(cm_pca, 1, sum)
colsums_pca <- apply(cm_pca, 2, sum)

accuracy_pca <- sum(diag_values_pca) / sum(cm_pca)
precision_pca <- diag_values_pca / colsums_pca
recall_pca <- diag_values_pca / rowsums_pca
f1_pca <- 2 * precision_pca * recall_pca / (precision_pca + recall_pca)

print("Classification Metrics on PCA Scores:")
print(data.frame(Recall = recall_pca, Precision = precision_pca, F1 = f1_pca))










