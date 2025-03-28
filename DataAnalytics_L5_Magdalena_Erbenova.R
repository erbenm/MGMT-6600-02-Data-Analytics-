#LAB 5  

#libraries
library(e1071)
library(caret)
library(class)
library(tidyverse)

#=========================#
#    PART 1: Wine.Data    #
#=========================#
#changing the column labels 
colnames(wine) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavinoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
head(wine)
View(wine)

#creating subset
wine_subset <- wine[, c("class", "Alcohol", "Flavinoids", "Color intensity", "Hue")]
wine_subset$class <- as.factor(wine_subset$class)

#splitting the subset into training and test set
set.seed(123)
train_index <- createDataPartition(wine_subset$class, p = 0.8, list = FALSE)
train_data <- wine_subset[train_index, ]
test_data <- wine_subset[-train_index, ]

# ------- SVM Linear Kernel -------
#adjust and train SVM with linear kernel
adj_linear <- tune.svm(class ~ ., data = train_data, kernel = "linear",
                       cost = 10^(-1:2))
svm_linear <- adj_linear$best.model

# ------- SVM Radial Kernel -------
#adjust and train SVM with radial kernel
adj_radial <- tune.svm(class ~ ., data = train_data, kernel = "radial",
                       cost = 10^(-1:2), gamma = 10^(-2:1))
svm_radial <- adj_radial$best.model

# ------- Naive Bayes Model -------
nb_model <- naiveBayes(class ~ ., data = train_data)

#===> PREDICT <===
pred_linear <- predict(svm_linear, test_data)
pred_radial <- predict(svm_radial, test_data)
pred_nb <- predict(nb_model, test_data)

#===> EVALUATION of the Performance <===
eval_model <- function(true, pred) {
  cm <- confusionMatrix(pred, true)
  precision <- cm$byClass[,"Precision"]
  recall <- cm$byClass[,"Recall"]
  f1 <- 2 * ((precision * recall) / (precision + recall))
  data.frame(Precision = mean(precision, na.rm=TRUE),
             Recall = mean(recall, na.rm=TRUE),
             F1 = mean(f1, na.rm=TRUE))
}

results <- rbind(
  Linear_SVM = eval_model(test_data$class, pred_linear),
  Radial_SVM = eval_model(test_data$class, pred_radial),
  Naive_Bayes = eval_model(test_data$class, pred_nb)
)

print(results)


# ------------- Results Table -------------
#            Precision    Recall        F1
#Linear_SVM  0.9458874 0.9458874 0.9458874
#Radial_SVM  0.9777778 0.9629630 0.9688979
#Naive_Bayes 0.9777778 0.9696970 0.9726327
#------------------------------------------

#==============================#
#   PART 2: NY_House_Dataset   #
#==============================#

#libraries
library(e1071)
library(ggplot2)

#renaming dataset
ny <- NY_House_Dataset_1_

#filtering data (keep only PRICE and PROPERTYSQFT)
ny <- ny[, c("PRICE", "PROPERTYSQFT")]
ny <- ny[complete.cases(ny), ]
ny <- ny[ny$PRICE > 0 & ny$PROPERTYSQFT > 0, ]  # Remove invalid rows

#price (log10)
ny$logPRICE <- log10(ny$PRICE)

#split into train/test
set.seed(123)
n <- nrow(ny)
index <- sample(1:n, 0.8 * n)
train <- ny[index, ]
test <- ny[-index, ]

# -------- SVM regression -------
svm_model <- svm(logPRICE ~ PROPERTYSQFT, data = train, type = "eps-regression")
svm_preds_log <- predict(svm_model, newdata = test)

# -------- linear regression --------
lm_model <- lm(logPRICE ~ PROPERTYSQFT, data = train)
lm_preds_log <- predict(lm_model, newdata = test)

# ------- plotting: predicted vs actual -------
dev.new(width = 10, height = 5)  #open a new plotting window with more space
par(mfrow = c(1, 2)) #1 row, 2 plots
par(mar = c(5, 5, 4, 2)) #reset margins

plot(test$logPRICE, svm_preds_log,
     main = "SVM: Predicted vs Actual",
     xlab = "Actual Price", ylab = "Predicted Price",
     pch = 16)
abline(0, 1, col = "red")

plot(test$logPRICE, lm_preds_log,
     main = "Linear: Predicted vs Actual",
     xlab = "Actual Price", ylab = "Predicted Price",
     pch = 16)
abline(0, 1, col = "blue")

# ------- residual plots -------
svm_residuals <- test$logPRICE - svm_preds_log
lm_residuals <- test$logPRICE - lm_preds_log

par(mfrow = c(1, 2)) # Residual plots
par(mar = c(5, 5, 4, 2))

plot(test$PROPERTYSQFT, svm_residuals,
     main = "SVM Residuals",
     xlab = "Square Footage", ylab = "Residuals", pch = 16)
abline(h = 0, col = "red")

plot(test$PROPERTYSQFT, lm_residuals,
     main = "Linear Residuals",
     xlab = "Square Footage", ylab = "Residuals", pch = 16)
abline(h = 0, col = "blue")

# -------------- Results --------------
#The SVM regression model outperformed the linear regression model in both prediction accuracy and residual distribution.
#It showed tighter clustering around the actual values and handled non-linear relationships in housing prices more effectively, especially across mid-range square footages. 
#In contrast, the linear model displayed larger residuals and more significant prediction errors, particularly at higher price points. 
#Overall, SVM proved to be a better fit for capturing the complexity of the data.



