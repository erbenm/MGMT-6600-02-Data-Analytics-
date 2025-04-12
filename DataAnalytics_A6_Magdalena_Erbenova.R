
#load packages
library(tidyverse)
library(DataExplorer)
library(caret)
library(corrplot)
library(pROC)
library(randomForest)
library(dplyr)

#==========================#
# 1. Exploratory Analysis  #
#==========================#

#renaming dataset 
dataset <- Bank_Target_Marketing_Dataset
dataset[dataset == "unknown"] <- NA
colSums(is.na(dataset)) #checking how many NAs per column

#droping columns with high NAs
dataset <- dataset %>%
  select(-contact, -poutcome)

#convert categorical variables to factors
dataset <- dataset %>%
  mutate(across(c(job, marital, education, default, housing, loan, month, deposit), as.factor))

#class balance of target variable
ggplot(dataset, aes(deposit)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Deposit Subscriptions", x = "Deposit", y = "Count")

#boxplot - Call Duration by Deposit
ggplot(dataset, aes(x = deposit, y = duration, fill = deposit)) +
  geom_boxplot() +
  labs(title = "Call Duration by Deposit Subscription", y = "Duration (seconds)")

#job type vs deposit 
ggplot(dataset, aes(x = job, fill = deposit)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Job Type vs. Deposit Subscription", y = "Proportion")

#summary of variables
dataset %>% select(where(is.numeric)) %>% summary()

#correlation plot
#select numeric columns
numeric_vars <- dataset %>% select(where(is.numeric))

#create correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

#plot
corrplot(cor_matrix, method = "square", tl.cex = 0.8)

#=======================#
# 2. Model Development  #
#=======================#
#----------------------------------------------------
## REGRESSION MODEL

#make sure deposit is a factor
dataset$deposit <- as.factor(dataset$deposit)

#remove duration
dataset_model <- dataset %>% select(-duration)

#drop rows with NAs 
dataset_model <- na.omit(dataset_model)

#set seed and split data
set.seed(123)
train_index <- createDataPartition(dataset_model$deposit, p = 0.7, list = FALSE)
train_data <- dataset_model[train_index, ]
test_data <- dataset_model[-train_index, ]

#fit logistic regression model
log_model <- glm(deposit ~ ., data = train_data, family = "binomial")

#predict probabilities on test data
log_probs <- predict(log_model, newdata = test_data, type = "response")

#convert to class labels using 0.5 threshold
log_preds <- ifelse(log_probs > 0.5, "yes", "no") %>% as.factor()

#confusion Matrix
conf_matrix <- confusionMatrix(log_preds, test_data$deposit)
print(conf_matrix)

#ROC Curve and AUC
log_roc <- roc(test_data$deposit, log_probs)
plot(log_roc, main = "ROC Curve - Logistic Regression", col = "blue")
auc(log_roc)
#-------------------------------------------------------------------
## RANDOM FOREST MODEL
#fit random forest model
set.seed(123)
rf_model <- randomForest(deposit ~ ., data = train_data, importance = TRUE, ntree = 500)

#predict class labels
rf_preds <- predict(rf_model, newdata = test_data)

#predict probabilities (needed for ROC/AUC)
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")[, 2]  # prob of 'yes'

#confusion Matrix
confusionMatrix(rf_preds, test_data$deposit)

#ROC & AUC
rf_roc <- roc(test_data$deposit, rf_probs)
plot(rf_roc, main = "ROC Curve - Random Forest", col = "darkgreen")
auc(rf_roc)

#plot feature importance
varImpPlot(rf_model)
#----------------------------------------------------------
## K-MEANS CLUSTERING MODEL
#select numeric features only (remove deposit!)
clust_data <- dataset %>%
  select(age, balance, campaign, pdays, previous) %>%
  drop_na() %>%
  scale()  # Normalize for distance-based clustering

#within-cluster sum of squares for k = 1 to 10
wss <- sapply(1:10, function(k) {
  kmeans(clust_data, centers = k, nstart = 10)$tot.withinss
})

#plot elbow curve
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Within-cluster Sum of Squares",
     main = "Elbow Method for Choosing k")

#fit K-Means with chosen number of clusters
set.seed(123)
kmeans_model <- kmeans(clust_data, centers = 4, nstart = 25)

#add cluster labels to the dataset
dataset$cluster <- as.factor(kmeans_model$cluster)

#cluster profile summary
dataset %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    avg_age = mean(age, na.rm = TRUE),
    avg_balance = mean(balance, na.rm = TRUE),
    avg_campaign = mean(campaign, na.rm = TRUE),
    avg_pdays = mean(pdays, na.rm = TRUE),
    avg_previous = mean(previous, na.rm = TRUE)
  )
#cross-tabulate clusters vs deposit outcome
table(dataset$cluster, dataset$deposit)

#bar plot of deposit rate per cluster
ggplot(dataset, aes(x = cluster, fill = deposit)) +
  geom_bar(position = "fill") +
  labs(title = "Deposit Rate by Cluster", y = "Proportion", x = "Cluster") +
  scale_fill_manual(values = c("no" = "salmon", "yes" = "seagreen"))
#--------------------------------------------------------------








