#########
# LAB 6 #
#########

library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(rpart)
library(cv)

# ------- Data Preparation -------
#renaming the dataset
dataset <- NY_House_Dataset_1_

#keep only necessary columns and clean NA
dataset <- na.omit(dataset[, c("PROPERTYSQFT", "PRICE")])

#exploring and plotting the dataset
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

#removing the outliers (using Lab 2 as a reference) 
dataset <- dataset[dataset$PROPERTYSQFT != 2184.207862, ]

#adding log-transformed variables
dataset$log_PRICE <- log10(dataset$PRICE)
dataset$log_SQFT <- log10(dataset$PROPERTYSQFT)

# ------- model 1 - Linear Regression -------
lm_model <- lm(log_PRICE ~ log_SQFT, data = dataset)
summary(cv(lm_model, K = 5))

#linear regression fit
ggplot(dataset, aes(x = log_SQFT, y = log_PRICE)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression Fit",
       x = "Square Footage", y = "Price")

# ------- model 2 - SVM (Linear Kernel) -------
svm_linear_model <- svm(log_PRICE ~ log_SQFT, data = dataset, kernel = "linear")

#SVM linear plotting
svm.mod <- svm(log_PRICE ~ log_SQFT, data = dataset, kernel = "linear")
svm.pred <- predict(svm.mod, dataset)

ggplot(dataset, aes(x = log_SQFT, y = log_PRICE)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = svm.pred), color = "green") +
  labs(title = "SVM (Linear Kernel) Fit Scale",
       x = "Square Footage", y = "Price")

# ------- model 3 - SVM (Radial Kernel) -------
svm_radial_model <- svm(log_PRICE ~ log_SQFT, data = dataset, kernel = "radial", gamma = 0.1, cost = 100)

#SVM radial plotting
svm.mod.radial <- svm(log_PRICE ~ log_SQFT, data = dataset, kernel = "radial", gamma = 0.1, cost = 100)
svm.pred.radial <- predict(svm.mod.radial, dataset)

ggplot(dataset, aes(x = log_SQFT, y = log_PRICE)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = svm.pred.radial), color = "red") +
  labs(title = "SVM (Radial Kernel) Fit Scale",
       x = "Square Footage", y = "Price")

# ------- collecting error metrics -------
set.seed(42)
n <- nrow(dataset)
folds <- sample(rep(1:5, length.out = n))

#metric vectors for each model
mae_lm <- mse_lm <- rmse_lm <- numeric(5)
mae_lin <- mse_lin <- rmse_lin <- numeric(5)
mae_rad <- mse_rad <- rmse_rad <- numeric(5)

for (k in 1:5) {
  test_idx <- which(folds == k)
  train_data <- dataset[-test_idx, ]
  test_data  <- dataset[test_idx, ]
  
  # --- linear regression ---
  lm_mod <- lm(log_PRICE ~ log_SQFT, data = train_data)
  pred_lm <- predict(lm_mod, test_data)
  true_lm <- test_data$log_PRICE
  mae_lm[k] <- mean(abs(pred_lm - true_lm))
  mse_lm[k] <- mean((pred_lm - true_lm)^2)
  rmse_lm[k] <- sqrt(mse_lm[k])
  
  # --- SVM linear ---
  svm_lin <- svm(log_PRICE ~ log_SQFT, data = train_data, kernel = "linear")
  pred_lin <- predict(svm_lin, test_data)
  true_lin <- test_data$log_PRICE
  mae_lin[k] <- mean(abs(pred_lin - true_lin))
  mse_lin[k] <- mean((pred_lin - true_lin)^2)
  rmse_lin[k] <- sqrt(mse_lin[k])
  
  # --- SVM radial ---
  svm_rad <- svm(log_PRICE ~ log_SQFT, data = train_data, kernel = "radial", gamma = 0.1, cost = 100)
  pred_rad <- predict(svm_rad, test_data)
  true_rad <- test_data$log_PRICE
  mae_rad[k] <- mean(abs(pred_rad - true_rad))
  mse_rad[k] <- mean((pred_rad - true_rad)^2)
  rmse_rad[k] <- sqrt(mse_rad[k])
}

# Summary results table
results <- data.frame(
  Model = c("Linear Regression", "SVM (Linear)", "SVM (Radial)"),
  MAE = c(mean(mae_lm), mean(mae_lin), mean(mae_rad)),
  MSE = c(mean(mse_lm), mean(mse_lin), mean(mse_rad)),
  RMSE = c(mean(rmse_lm), mean(rmse_lin), mean(rmse_rad))
)
#printing a table with results
print(results)


# ------- table w/ results -------
#           Model       MAE        MSE      RMSE
#1 Linear Regression 0.2329807 0.08558558 0.2923624
#2      SVM (Linear) 0.2284817 0.08883723 0.2978277
#3      SVM (Radial) 0.2252517 0.08558061 0.2923124