#instal package
install.packages("car")
install.packages("naivebayes")

#load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(e1071) #Naive Bayes
library(naivebayes)
library(class) #k-NN
library(randomForest) #Random Forest
library(GGally)
library(car)

#view the dataset
NYCdata <- NYC_Citywide_Annualized_Calendar_Sales_Update_20241107
View(NYCdata)
str(NYCdata)

#derivate subset Bronx (2)
bronx_df <- NYCdata %>% filter(BOROUGH == 2)

#=========
#   Q1a
#=========
#This analysis explores how property size (LAND SQUARE FEET, GROSS SQUARE FEET) and YEAR BUILT impact sale price in the Bronx. 
#I also examine neighborhood price variations to identify high-value areas.
#To explore these trends, I used histograms and boxplots to detect outliers and cleaned the dataset by removing unrealistic values.
#after initial analysis I decided to add Latitude and Longtitude to the analysis to see if the LM model would imporve.

#=========
#   Q1b
#=========
# Filtering the data
bronx_df <- bronx_df %>% filter(!is.na(`SALE PRICE`) & `SALE PRICE` > 1000)
upper_limit <- quantile(bronx_df$`SALE PRICE`, 0.99, na.rm = TRUE)
bronx_df <- bronx_df %>% filter(`SALE PRICE` <= upper_limit)

# Check for missing values in key columns
colSums(is.na(bronx_df[, c("SALE PRICE", "LAND SQUARE FEET", "GROSS SQUARE FEET", "YEAR BUILT")]))

# Replace missing values in numeric columns with the median
bronx_df$`LAND SQUARE FEET`[is.na(bronx_df$`LAND SQUARE FEET`)] <- median(bronx_df$`LAND SQUARE FEET`, na.rm = TRUE)
bronx_df$`GROSS SQUARE FEET`[is.na(bronx_df$`GROSS SQUARE FEET`)] <- median(bronx_df$`GROSS SQUARE FEET`, na.rm = TRUE)
bronx_df$`YEAR BUILT`[is.na(bronx_df$`YEAR BUILT`)] <- median(bronx_df$`YEAR BUILT`, na.rm = TRUE)

# Check again for missing values
colSums(is.na(bronx_df[, c("SALE PRICE", "LAND SQUARE FEET", "GROSS SQUARE FEET", "YEAR BUILT")]))

# Calculate Q1, Q3, and IQR
Q1 <- quantile(bronx_df$`SALE PRICE`, 0.25, na.rm = TRUE)
Q3 <- quantile(bronx_df$`SALE PRICE`, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter out extreme values
bronx_df <- bronx_df %>% filter(`SALE PRICE` >= lower_bound & `SALE PRICE` <= upper_bound)

# Histogram to visualize the distribution of sale prices
ggplot(bronx_df, aes(x = `SALE PRICE`)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "green", alpha = 0.7) +
  scale_y_log10() +  # Log scale to better visualize distribution
  labs(title = "Distribution of Sale Prices in Bronx",
       x = "Sale Price ($)", y = "Frequency (log scale)") +
  theme_minimal()

# Boxplot to identify outliers in sale prices
ggplot(bronx_df, aes(y = `SALE PRICE`)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Boxplot of Sale Prices in Bronx",
       y = "Sale Price ($)") +
  theme_minimal()
#I first checked for missing values and then replaced them with median. 
#Then I looked at the distrubution of sale prices using histogram, which revealed a highly skewed distribution with extreme sale prices.
#A boxplot confirmed the presence of significant outliers, where some properties had sale prices far above the typical range.
#To improve the data quality, I removed outliers using the IQR method.
#After cleaning, the histogram showed a more balanced distribution, and the updated boxplot no longer contained extreme values, making the dataset more suitable for modeling. 
#These steps ensure that the upcoming regression model will be more accurate and less influenced by extreme data points.

#=========
#   Q1c
#=========
#Create dummy variable for NEIGHBORHOOD and BUILDING CLASS CATEGORY
NEIGHBORHOOD_DUMMIES <- model.matrix(~ NEIGHBORHOOD - 1, data = bronx_df)  # Create dummies for NEIGHBORHOOD
BUILDING_DUMMIES <- model.matrix(~ `BUILDING CLASS CATEGORY` - 1, data = bronx_df)  # Create dummies for BUILDING CLASS CATEGORY

#add these variables to the dataset
bronx_df <- cbind(bronx_df, NEIGHBORHOOD_DUMMIES, BUILDING_DUMMIES)

# Create SALE YEAR variable 
bronx_df$`SALE YEAR` <- as.numeric(format(as.Date(bronx_df$`SALE DATE`, format="%m/%d/%Y"), "%Y"))

# Regression model
model1 <- lm(log(`SALE PRICE`) ~ `GROSS SQUARE FEET` + `YEAR BUILT` + `SALE YEAR` + 
               NEIGHBORHOOD_DUMMIES + BUILDING_DUMMIES, data = bronx_df)
# Summary
summary(model1)
#-------------------------------------------------------
# Derive subset from BRONX subset = RIVERDALE
bronx_subset <- bronx_df %>% filter(NEIGHBORHOOD == "RIVERDALE")

# Checking # of properties in the subset
nrow(bronx_subset)

# Predict sale prices using the final model
bronx_subset$predicted_price <- predict(model1, newdata = bronx_subset)

# Convert predicted prices back to the original scale
bronx_subset$predicted_price <- exp(bronx_subset$predicted_price)

# Compare actual vs. predicted sale prices
head(bronx_subset[, c("SALE PRICE", "predicted_price")])

# Calculate residuals (difference between actual and predicted prices)
bronx_subset$residuals <- bronx_subset$`SALE PRICE` - bronx_subset$predicted_price

# Plot actual vs. predicted sale prices
ggplot(bronx_subset, aes(x = predicted_price, y = `SALE PRICE`)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Sale Prices (Morris Heights)",
       x = "Predicted Sale Price ($)", y = "Actual Sale Price ($)") +
  theme_minimal()

#With these predictores this model achieves and R-squared of ~ 44%, meaning it explains 44% of the variation in sale prices.
#All predictors were statistically significant, confirming that property size, location, building type strongly, and etc. impact pricing.
#Also applying a log to SALE PRICE reduces skewness and improves model stability.
#To test the model, we applied it to a subset of properties in the Riverdale neighborhood. The model successfully predicted general pricing trends, but deviations in actual vs. predicted sale prices suggest that some high-value properties are underestimated.
#The scatter plot revealed some clustering at certain price levels, indicating that additional variables could further refine predictions.
#Overall, the model provides somewhat strong baseline for price prediction but still has room for refinement.

#=========
#   Q1d
#=========
# Select relevant columns and remove missing values
bronx_clean <- bronx_df %>%
  select(NEIGHBORHOOD, SALE PRICE, GROSS SQUARE FEET, LAND SQUARE FEET, 
         TOTAL UNITS, RESIDENTIAL UNITS, YEAR BUILT, TAX CLASS AT TIME OF SALE, Latitude, Longitude) %>%
  na.omit() 

# Rename columns to remove spaces
bronx_clean <- bronx_clean %>%
  rename(SALE_PRICE = SALE PRICE,
         GROSS_SQUARE_FEET = GROSS SQUARE FEET,
         LAND_SQUARE_FEET = LAND SQUARE FEET,
         TOTAL_UNITS = TOTAL UNITS,
         RESIDENTIAL_UNITS = RESIDENTIAL UNITS,
         YEAR_BUILT = YEAR BUILT,
         TAX_CLASS = TAX CLASS AT TIME OF SALE)

# Convert NEIGHBORHOOD into a factor (classification target)
bronx_clean$NEIGHBORHOOD <- as.factor(bronx_clean$NEIGHBORHOOD)

# Split data into training (70%) and testing (30%)
set.seed(123)
trainIndex <- createDataPartition(bronx_clean$NEIGHBORHOOD, p = 0.7, list = FALSE)
trainData <- bronx_clean[trainIndex, ]
testData <- bronx_clean[-trainIndex, ]

# -------------------------
# MODEL 1: Random Forest
# -------------------------

rf_model <- randomForest(NEIGHBORHOOD ~ SALE_PRICE + GROSS_SQUARE_FEET + LAND_SQUARE_FEET + 
                           TOTAL_UNITS + RESIDENTIAL_UNITS + YEAR_BUILT + TAX_CLASS + 
                           Latitude + Longitude, 
                         data = trainData, ntree = 100)

rf_predictions <- predict(rf_model, testData)
rf_cm <- confusionMatrix(rf_predictions, testData$NEIGHBORHOOD)

# -------------------------
# MODEL 2: k-Nearest Neighbors (k-NN)
# -------------------------

# Normalize numerical features for k-NN
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
bronx_clean[, 2:ncol(bronx_clean)] <- as.data.frame(lapply(bronx_clean[, 2:ncol(bronx_clean)], normalize))

# Split again after normalization
trainData <- bronx_clean[trainIndex, ]
testData <- bronx_clean[-trainIndex, ]

# Train k-NN model
knn_model <- train(NEIGHBORHOOD ~ ., data = trainData, method = "knn", tuneGrid = data.frame(k = 5))
knn_predictions <- predict(knn_model, testData)
knn_cm <- confusionMatrix(knn_predictions, testData$NEIGHBORHOOD)

# -------------------------
# MODEL 3: Naïve Bayes
# -------------------------

nb_model <- naive_bayes(NEIGHBORHOOD ~ ., data = trainData)
nb_predictions <- predict(nb_model, testData)
nb_cm <- confusionMatrix(nb_predictions, testData$NEIGHBORHOOD)

# -------------------------
# COMPARE MODEL PERFORMANCE
# -------------------------

# Extract Accuracy for all models
accuracy_rf <- rf_cm$overall["Accuracy"]
accuracy_knn <- knn_cm$overall["Accuracy"]
accuracy_nb <- nb_cm$overall["Accuracy"]

# Print Accuracy Results
print(paste("Accuracy - Random Forest:", round(as.numeric(accuracy_rf), 4)))
print(paste("Accuracy - k-NN:", round(as.numeric(accuracy_knn), 4)))
print(paste("Accuracy - Naïve Bayes:", round(as.numeric(accuracy_nb), 4)))

# -------------------------
# Evaluate Precision, Recall, and F1-Score
# -------------------------

# Function to calculate evaluation metrics
evaluate_model <- function(cm) {
  accuracy <- sum(diag(cm$table)) / sum(cm$table)  # Overall accuracy
  precision <- diag(cm$table) / colSums(cm$table)  # Precision per class
  recall <- diag(cm$table) / rowSums(cm$table)  # Recall per class
  f1 <- 2 * (precision * recall) / (precision + recall)  # F1 score
  
  return(data.frame(Precision=precision, Recall=recall, F1_Score=f1))
}

# Evaluate each model
rf_eval <- evaluate_model(rf_cm)
knn_eval <- evaluate_model(knn_cm)
nb_eval <- evaluate_model(nb_cm)

# Print results
print("Performance - Random Forest:")
print(rf_eval)

print("Performance - k-NN:")
print(knn_eval)

print("Performance - Naïve Bayes:")
print(nb_eval)

#The dataset was cleaned by removing missing values, renaming columns, and normalizing numerical features for k-NN.
#Random Forest performed best (98.21% accuracy), with high precision and recall across neighborhoods. 
#k-NN (92.62%) struggled with boundary neighborhoods, while Naïve Bayes (79.38%) performed the worst due to predictor dependencies. 
#Contingency tables showed NaN recall values for smaller neighborhoods, likely due to insufficient samples.
#Latitude/Longitude significantly improved classification. 
#Random Forest was the most reliable model, and future improvements could involve merging underrepresented neighborhoods or using advanced classifiers.
##################################################################################################################################################################
# Q2
#=========
#   Q2a
#=========
# Select Brooklyn data
brooklyn_df <- NYCdata %>% filter(BOROUGH == 3)

# Convert categorical variables to character first
brooklyn_df$NEIGHBORHOOD <- as.character(brooklyn_df$NEIGHBORHOOD)
brooklyn_df$`BUILDING CLASS CATEGORY` <- as.character(brooklyn_df$`BUILDING CLASS CATEGORY`)

# Display unique values (for debugging)
cat("Unique NEIGHBORHOODS before filtering:\n")
print(unique(brooklyn_df$NEIGHBORHOOD))
cat("Unique BUILDING CLASS CATEGORIES before filtering:\n")
print(unique(brooklyn_df$`BUILDING CLASS CATEGORY`))
cat("Rows before filtering:", nrow(brooklyn_df), "\n")

# Clean and Filter Brooklyn Data
brooklyn_df <- brooklyn_df %>% filter(!is.na(`SALE PRICE`) & `SALE PRICE` > 1000)
upper_limit <- quantile(brooklyn_df$`SALE PRICE`, 0.99, na.rm = TRUE)
brooklyn_df <- brooklyn_df %>% filter(`SALE PRICE` <= upper_limit)

# Replace missing numeric values with medians
brooklyn_df$`LAND SQUARE FEET`[is.na(brooklyn_df$`LAND SQUARE FEET`)] <- 
  median(brooklyn_df$`LAND SQUARE FEET`, na.rm = TRUE)
brooklyn_df$`GROSS SQUARE FEET`[is.na(brooklyn_df$`GROSS SQUARE FEET`)] <- 
  median(brooklyn_df$`GROSS SQUARE FEET`, na.rm = TRUE)
brooklyn_df$`YEAR BUILT`[is.na(brooklyn_df$`YEAR BUILT`)] <- 
  median(brooklyn_df$`YEAR BUILT`, na.rm = TRUE)

# Create SALE YEAR variable
brooklyn_df$`SALE YEAR` <- as.numeric(format(as.Date(brooklyn_df$`SALE DATE`, format="%m/%d/%Y"), "%Y"))

cat("Rows after filtering:", nrow(brooklyn_df), "\n")

# Convert categorical variables to factors
brooklyn_df$NEIGHBORHOOD <- as.factor(brooklyn_df$NEIGHBORHOOD)
brooklyn_df$`BUILDING CLASS CATEGORY` <- as.factor(brooklyn_df$`BUILDING CLASS CATEGORY`)

# Create Dummy Variables for Brooklyn (Matching Bronx)
brooklyn_neigh <- model.matrix(~ NEIGHBORHOOD - 1, data = brooklyn_df)
brooklyn_neigh <- as.data.frame(brooklyn_neigh)

brooklyn_bldg <- model.matrix(~ `BUILDING CLASS CATEGORY` - 1, data = brooklyn_df)
brooklyn_bldg <- as.data.frame(brooklyn_bldg)

# Ensure Brooklyn has all dummies present in Bronx model
required_neigh_names <- colnames(NEIGHBORHOOD_DUMMIES)
required_bldg_names <- colnames(BUILDING_DUMMIES)

# Add missing dummy columns (fill with 0)
for(col in required_neigh_names) {
  if(!(col %in% colnames(brooklyn_neigh))) {
    brooklyn_neigh[[col]] <- 0
  }
}
brooklyn_neigh <- brooklyn_neigh[, required_neigh_names, drop = FALSE]

for(col in required_bldg_names) {
  if(!(col %in% colnames(brooklyn_bldg))) {
    brooklyn_bldg[[col]] <- 0
  }
}
brooklyn_bldg <- brooklyn_bldg[, required_bldg_names, drop = FALSE]

# Merge dummies into Brooklyn Data
brooklyn_df <- cbind(brooklyn_df, brooklyn_neigh, brooklyn_bldg)

# Ensure all required predictors exist in Brooklyn
required_columns <- names(coef(model1))  # Get all predictor names from Bronx model
missing_columns <- setdiff(required_columns, colnames(brooklyn_df))  # Find missing columns

# Add missing predictors as zeros
for (col in missing_columns) {
  brooklyn_df[[col]] <- 0
}

# Ensure column order matches the model
brooklyn_regression_data <- brooklyn_df[, required_columns, drop = FALSE]

#Up until now everything works, however, I have trouble to match the model1 to the Brooklyn subset
# Apply Regression Model
brooklyn_df$predicted_price <- predict(model1, newdata = brooklyn_regression_data)
brooklyn_df$predicted_price <- exp(brooklyn_df$predicted_price)  # Convert from log scale

# Remove rows with NA predictions
brooklyn_df <- brooklyn_df %>% filter(!is.na(predicted_price))

# Calculate residuals
brooklyn_df$residuals <- brooklyn_df$`SALE PRICE` - brooklyn_df$predicted_price

# Plot Results
ggplot(brooklyn_df, aes(x = predicted_price, y = `SALE PRICE`)) +
  geom_point(color = "pink", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Sale Prices (Brooklyn)",
       x = "Predicted Sale Price ($)", y = "Actual Sale Price ($)") +
  theme_minimal()

ggplot(brooklyn_df, aes(x = predicted_price, y = residuals)) +
  geom_point(color = "orange", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot (Brooklyn)",
       x = "Predicted Sale Price ($)", y = "Residuals") +
  theme_minimal()

#=========
#   Q2b - Applying Classification Models to Brooklyn
#=========
# Load and preprocess Brooklyn data
brooklyn_clean <- NYCdata %>% filter(BOROUGH == 3) %>%
  select(NEIGHBORHOOD, `SALE PRICE`, `GROSS SQUARE FEET`, `LAND SQUARE FEET`, 
         `TOTAL UNITS`, `RESIDENTIAL UNITS`, `YEAR BUILT`, `TAX CLASS AT TIME OF SALE`, `Latitude`, `Longitude`) %>%
  na.omit()

# Rename columns for consistency
brooklyn_clean <- brooklyn_clean %>%
  rename(SALE_PRICE = `SALE PRICE`,
         GROSS_SQUARE_FEET = `GROSS SQUARE FEET`,
         LAND_SQUARE_FEET = `LAND SQUARE FEET`,
         TOTAL_UNITS = `TOTAL UNITS`,
         RESIDENTIAL_UNITS = `RESIDENTIAL UNITS`,
         YEAR_BUILT = `YEAR BUILT`,
         TAX_CLASS_AT_TIME_OF_SALE = `TAX CLASS AT TIME OF SALE`)

# Convert NEIGHBORHOOD to factor
brooklyn_clean$NEIGHBORHOOD <- as.factor(brooklyn_clean$NEIGHBORHOOD)

# Normalize numerical features for k-NN (as done in Q1d)
brooklyn_clean[, 2:ncol(brooklyn_clean)] <- as.data.frame(lapply(brooklyn_clean[, 2:ncol(brooklyn_clean)], normalize))

# Split Brooklyn data (70% training, 30% testing)
set.seed(123)
trainIndex_b <- createDataPartition(brooklyn_clean$NEIGHBORHOOD, p = 0.7, list = FALSE)
trainData_b <- brooklyn_clean[trainIndex_b, ]
testData_b <- brooklyn_clean[-trainIndex_b, ]

# -------------------------
# APPLY MODEL 1: Random Forest
# -------------------------
rf_predictions_b <- predict(rf_model, testData_b)
rf_cm_b <- confusionMatrix(rf_predictions_b, testData_b$NEIGHBORHOOD)

# -------------------------
# APPLY MODEL 2: k-Nearest Neighbors (k-NN)
# -------------------------
knn_predictions_b <- predict(knn_model, testData_b)
knn_cm_b <- confusionMatrix(knn_predictions_b, testData_b$NEIGHBORHOOD)

# -------------------------
# APPLY MODEL 3: Naïve Bayes
# -------------------------
nb_predictions_b <- predict(nb_model, testData_b)
nb_cm_b <- confusionMatrix(nb_predictions_b, testData_b$NEIGHBORHOOD)

# -------------------------
# COMPARE MODEL PERFORMANCE
# -------------------------

# Extract Accuracy for all models
accuracy_rf_b <- rf_cm_b$overall["Accuracy"]
accuracy_knn_b <- knn_cm_b$overall["Accuracy"]
accuracy_nb_b <- nb_cm_b$overall["Accuracy"]

# Print Accuracy Results
print(paste("Accuracy - Random Forest (Brooklyn):", round(as.numeric(accuracy_rf_b), 4)))
print(paste("Accuracy - k-NN (Brooklyn):", round(as.numeric(accuracy_knn_b), 4)))
print(paste("Accuracy - Naïve Bayes (Brooklyn):", round(as.numeric(accuracy_nb_b), 4)))

# -------------------------
# Evaluate Precision, Recall, and F1-Score
# -------------------------

# Evaluate each model
rf_eval_b <- evaluate_model(rf_cm_b)
knn_eval_b <- evaluate_model(knn_cm_b)
nb_eval_b <- evaluate_model(nb_cm_b)

# Print results
print("Performance - Random Forest (Brooklyn):")
print(rf_eval_b)

print("Performance - k-NN (Brooklyn):")
print(knn_eval_b)

print("Performance - Naïve Bayes (Brooklyn):")
print(nb_eval_b)

#Cannot add anything to the conclusion because my model does not work on other dataset
#I have spend at least 4h on this and I still could not figure our how to solve this :(
#I also encountered the same problem with the Q2b part...I tried to create a dummies variables to generalize the models so that they can be used on other boroughs, however, I failed. 
#I can possibly look at it again once i refresh, however, that would mean that I would be late with the submission.
#the code I included even though it does not work is the initial code
#I did not want to include everything because the code would be too long and it still does not work 

