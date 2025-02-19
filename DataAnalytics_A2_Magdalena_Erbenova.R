#installing packages
install.packages("ggplot2")
install.packages("readr")
install.packages("readxl")
install.packages("dplyr")

#load necessary libraries
library("ggplot2")
library("readr")
library("readxl")
library("dplyr")

#rename dataset & view
dataset<- epi_results_2024
##View(dataset)
head(dataset)
colnames(dataset)

#Attach dataset for easy column access 
attach(dataset)
#========================================================================================================================
#1. VARIABLE DISTRIBUTION
#define regions
##Eastern Europe
subset1 <- dataset[dataset$region == "Eastern Europe", ]
head(subset1)
##Global West
subset2 <- dataset[dataset$region == "Global West", ]
head(subset2)

#remove missing values - NOT NEEDED?
subset1 <- subset1[!is.na(subset1$EPI.new), ]
subset2 <- subset2[!is.na(subset2$EPI.new), ]

#checking the data structure after cleaning
head(subset1)
head(subset2)
nrow(subset1)
nrow(subset2)

#1.1 creating histrogram 
##Eastern Europe
hist(subset1$EPI.new, seq(10, 90, 4), prob=TRUE,main = "Histogram: Eastern Europe", xlab = "EPI.new Score") #adding name of the histogram and axis
lines(density(subset1$EPI.new, na.rm=TRUE, bw="SJ"), col="blue",) #SJ is for smoothing out the denisty curve
rug(subset1$EPI.new) 

##Global West
hist(subset2$EPI.new, seq(10, 90, 4), prob=TRUE,main = "Histogram: Global West", xlab = "EPI.new Score") #adding name of the histogram and axis
lines(density(subset2$EPI.new, na.rm=TRUE, bw="SJ"), col="maroon",) #SJ is for smoothing out the denisty curve
rug(subset2$EPI.new) 

#1.2 QQ plots 
##Eastern Europe
qqnorm(subset1$EPI.new, main="QQ Plot for EPI.new - Eastern Europe")
qqline(subset1$EPI.new, col="blue") 
##Global West
qqnorm(subset2$EPI.new, main="QQ Plot for EPI.new - Global West")
qqline(subset2$EPI.new, col="green")
#===============================================================================
#2. LINEAR MODELS  
#cleaning & filtering data
str(dataset$population) ##checking if population is numeric
dataset$population <- as.numeric(gsub(",","", dataset$population)) ##converting population to numerical value
subset <- dataset %>% filter(region == "Eastern Europe") ##creating subset for Eastern Europe
dataset <- dataset %>%
  filter(!is.na(EPI.new), !is.na(ECO.new), !is.na(population), population > 0) ##filtering out NAs values
#-------------------------------------------------------------------------------
#Fitting the Model 1 - ECO.new
model1 <- lm(ECO.new ~ log10(population), data = dataset)

#printing the Model 1 outputs
summary(model1)

#scatter plot for Model 1
ggplot(dataset, aes(x = log10(population), y = ECO.new)) + 
  geom_point() + 
  geom_smooth(method = "lm", col="purple")

#plotting residual for Model 1
residuals1 <- resid(model1)
fitted_values1 <- fitted(model1)
plot(fitted_values1, residuals1,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values using ECO.new",)
abline(h=0, col ="purple",)
#-------------------------------------------------------------------------------
#Fitting the Model 1a - EPI.new
model1a <- lm(EPI.new ~ log10(population), data = dataset)

#printing the Model 1a outputs
summary(model1a)

#scatter plot for Model 1a
ggplot(dataset, aes(x = log10(population), y = EPI.new)) + 
  geom_point() + 
  geom_smooth(method = "lm", col="yellow")

#plotting residual for Model 1a
residuals1a <- resid(model1a)
fitted_values1a <- fitted(model1a)
plot(fitted_values1a, residuals1a,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values using EPI.new",)
abline(h=0, col ="yellow",)
#-------------------------------------------------------------------------------
#making sure Eastern Europe subset is properly filtred
subset <- dataset %>% filter(region == "Eastern Europe")
#-------------------------------------------------------------------------------
#Fitting the Model 2 - Eastern Europe subset (ECO.new)
model2 <- lm(ECO.new ~ log10(population), data = subset)

#printing the Model 2 outputs
summary(model2)

#scatter plot for Model 2
ggplot(subset, aes(x = log10(population), y = ECO.new)) + 
  geom_point() + 
  geom_smooth(method = "lm", col="blue")

#plotting residual for Model 2
residuals2 <- resid(model2)
fitted_values2 <- fitted(model2)
plot(fitted_values2, residuals2,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values using ECO.new",)
abline(h=0, col ="blue",)
#-------------------------------------------------------------------------------
#Fitting the Model 2a - Eastern Europe subset (EPI.new)
model2a <- lm(EPI.new ~ log10(population), data = subset)

#printing the Model 2a outputs
summary(model2a)

#scatter plot for Model 2a
ggplot(subset, aes(x = log10(population), y = EPI.new)) + 
  geom_point() + 
  geom_smooth(method = "lm", col="maroon")

#plotting residual for Model 2a
residuals2a <- resid(model2a)
fitted_values2a <- fitted(model2a)
plot(fitted_values2a, residuals2a,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values using EPI.new",)
abline(h=0, col ="maroon",)

##SUMMARY
#Neither model provides a strong fit, as indicated by the low R² values in both 
#the full dataset and Eastern Europe subset. However, the full dataset model 
#for EPI.new shows a weak but significant relationship between population and 
#environmental performance (p = 0.01549), whereas in Eastern Europe, the relationship 
#is non-significant (p = 0.6311). This suggests that population is a more relevant 
#predictor globally, but within Eastern Europe, other factors likely drive environmental performance.
#==================================================================================================================
#3. CLASSIFICATION (kNN)

#install necessary packages 
install.packages("class") #for kNN model
install.packages("caret") #for confusion matrix & accuracy evaluation

#load necessary libraries
library(class)
library(caret)
library(dplyr)

#kNN Model 1
#creating a subset using two regions: Eastern Europe & Global West
subset_knn <- dataset %>%
  filter(region %in% c("Eastern Europe", "Global West")) %>%
  select(ECO.new, EPI.new, BDH.new, region) %>%
  na.omit()  # Remove missing values

#converting region to a factor > needed for classification 
subset_knn$region <- as.factor(subset_knn$region)

#checking structure
str(subset_knn)

#normalizing data using min & max
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#applying normalization to numerical column
subset_knn[ , 1:3] <- as.data.frame(lapply(subset_knn[ , 1:3], normalize))

#checking after normalization
summary(subset_knn$EPI.new)
summary(subset_knn$ECO.new)
summary(subset_knn$BDH.new)
# after normalization each variable is in between 0 and 1

# Split dataset into training (70%) and testing (30%)
set.seed(123)  # Ensures reproducibility
ind <- sample(2, nrow(subset_knn), replace = TRUE, prob = c(0.7, 0.3))
KNNtrain <- subset_knn[ind == 1, ]
KNNtest <- subset_knn[ind == 2, ]

#choosing k value based on the sqrt of sample size
sqrt(nrow(KNNtrain)) #output suggestion k=3

#train kNN model using selected features
k_value <- 3
KNN_predictions <- knn(train = KNNtrain[, 1:3],  # ECO.new, EPI.new, BDH.new
               test = KNNtest[, 1:3],    # ECO.new, EPI.new, BDH.new
               cl = KNNtrain$region,     # Target variable (Region)
               k = k_value)              # k = 3

#creating confusion matrix
cf_matrix <- confusionMatrix(as.factor(KNN_predictions), as.factor(KNNtest$region))
print(cf_matrix)

#calculating model accuracy 
accuracy <- sum(KNN_predictions == KNNtest$region)/length(KNNtest$region)
print(paste("Accuracy:", round(accuracy*100, 2), "%"))
#-------------------------------------------------------------------------------
#kNN Model 2
#creating a subset using two regions: Eastern Europe & Global West
subset_knn2 <- dataset %>%
  filter(region %in% c("Eastern Europe", "Global West")) %>%
  select(AIR.new, PSU.new , AGR.new, region) %>%
  na.omit()  # Remove missing values

#converting region to a factor > needed for classification 
subset_knn2$region <- as.factor(subset_knn2$region)

#checking structure & converting to numeric
str(subset_knn2)
subset_knn2$PSU.new <- as.numeric(as.character(subset_knn2$PSU.new))

#normalizing data using min & max
normalize2 <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#applying normalization to numerical column
subset_knn2[ , 1:3] <- as.data.frame(lapply(subset_knn2[ , 1:3], normalize))

#checking after normalization
summary(subset_knn2$AIR.new)
summary(subset_knn2$PSU.new)
summary(subset_knn2$AGR.new)
# after normalization each variable is in between 0 and 1

# Split dataset into training (70%) and testing (30%)
set.seed(123)  # Ensures reproducibility
ind2 <- sample(2, nrow(subset_knn2), replace = TRUE, prob = c(0.7, 0.3))
KNNtrain2 <- subset_knn2[ind2 == 1, ]
KNNtest2 <- subset_knn2[ind2 == 2, ]

#choosing k value based on the sqrt of sample size
sqrt(nrow(KNNtrain2)) #output suggestion k=6

#train kNN model using selected features
k_value2 <- 3
KNN_predictions2 <- knn(train = KNNtrain2[, 1:3],  # AIR.new, PSU.new, AGR.new
                       test = KNNtest2[, 1:3],    # AIR.new, PSU.new, AGR.new
                       cl = KNNtrain2$region,     # Target variable (Region)
                       k = k_value2)              # k = 3

#creating confusion matrix
cf_matrix2 <- confusionMatrix(as.factor(KNN_predictions2), as.factor(KNNtest2$region))
print(cf_matrix2)

#calculating model accuracy 
accuracy2 <- sum(KNN_predictions2 == KNNtest2$region)/length(KNNtest2$region)
print(paste("Accuracy:", round(accuracy2*100, 2), "%"))

##SUMMARY
#The second kNN model (Accuracy = 100%) outperformed the first model (Accuracy = 92.86%), 
#achieving perfect classification. However, the perfect accuracy suggests possible overfitting, 
#meaning the model may not work well with unseen data. Therefore, the second model appears 
#better in this specific case, however, further validation would need to be done to confirm 
#its reliability.
