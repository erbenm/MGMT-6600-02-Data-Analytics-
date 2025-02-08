# Install libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
#==========================================================================================
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
#==========================================================================================
# Rename dataset
nydata <- NY_House_Dataset_1_

# View dataset structure and summary
str(nydata)
summary(nydata )
colnames (nydata)
#==========================================================================================
# Check for missing values
sum(is.na(nydata$BATH))  
sum(is.nan(nydata$BATH))  
sum(is.infinite(nydata$BATH)) 
#==========================================================================================
## Filter data
nydata <- nydata[nydata$BATH>0,]
nydata <- nydata[nydata$BEDS<25,]
nydata <- nydata[nydata$PRICE<195000000,]
nydata$BATH <- as.numeric(nydata$BATH)
nydata <- na.omit(nydata)
nydata <- nydata[nydata$PROPERTYSQFT!=2184.207862,]
nydata$PROPERTYSQFT[nydata$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]# print Model1 output
??^
#==========================================================================================
##MODEL1
# Model 1: PRICE ~ PROPERTYSQFT
model1 <- lm(PRICE ~ PROPERTYSQFT, data = nydata)
model1 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), data = nydata)

# Scatter plot of 2 variables - model 1
plot(PRICE~PROPERTYSQFT, data = nydata)
abline(model1)
plot(log10(PRICE)~log10(PROPERTYSQFT), data = nydata)
abline(model1)

# Model1: GGPLOT
ggplot(nydata, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Model 1: Log(PROPERTYSQFT) vs Log(PRICE)")

# Residual Plot for Model 1
ggplot(data.frame(fitted = fitted(model1), residuals = resid(model1)), 
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot - Model 1", x = "Fitted Values", y = "Residuals")

#==========================================================================================
##MODEL2
# Model 2: PRICE ~ PROPERTYSQFT + BEDS
model2 <- lm(PRICE ~ PROPERTYSQFT + BEDS, data = nydata)
model2 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT) + log10(BEDS), data = nydata)

#Model2: GGPLOT
ggplot(nydata, aes(x = log10(PROPERTYSQFT), y = log10(PRICE), color = log10(BEDS))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Model 2: Log(PROPERTYSQFT) vs Log(PRICE) with BEDS as Color")

# Residual Plot for Model 2
ggplot(data.frame(fitted = fitted(model2), residuals = resid(model2)), 
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot - Model 2", x = "Fitted Values", y = "Residuals")

#==========================================================================================
##MODEL3
# Model3: Price ~ BATH + PROPERTYSQFT
model3 <- lm(PRICE ~ BATH + PROPERTYSQFT, data = nydata)
model3 <- lm(log10(PRICE) ~ BATH + log10(PROPERTYSQFT), data = nydata)

ggplot(nydata, aes(x = log10(PROPERTYSQFT), y = log10(PRICE), color = BATH)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Model 3: Log(PROPERTYSQFT) vs Log(PRICE), Colored by BATH")

# Residual Plot for Model 3
ggplot(data.frame(fitted = fitted(model3), residuals = resid(model3)), 
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot - Model 3", x = "Fitted Values", y = "Residuals")
#==========================================================================================

#Summaries of all three models
summary(model1)
summary(model2)
summary(model3)

# Model comparison
#Model 1 (PRICE ~ PROPERTYSQFT): Adjusted R² = 0.5829
#Model 2 (PRICE ~ PROPERTYSQFT + BEDS): Adjusted R² = 0.5880 (Best model)
#Model 3 (PRICE ~ PROPERTYSQFT + BATH): Adjusted R² = 0.5841
#It has the highest Adjusted R² (0.5880), meaning it explains the most variance in PRICE.
#It has the lowest Residual Standard Error (0.2865), meaning better predictions.
#Both predictors (PROPERTYSQFT & BEDS) are statistically significant (p-values < 0.05).
#Adding BEDS improves model performance slightly compared to Model 1.

#Conclusion: Model 2 is the best as it explains the most variance in PRICE with the lowest error.


#==========================================================================================
##MODEL4 *** EXTRA
# Model4: Price ~ BEDS
model4 <- lm(PRICE ~ BEDS, data = nydata)
model4 <- lm(log10(PRICE) ~ BEDS, data = nydata)

#Model3: GGPLOT
ggplot(nydata, aes(x = BEDS, y = log10(PRICE))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.6) +  # Adds small random noise to separate points
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Model 4: BEDS vs Log(PRICE)")

#summary of Model4
summary(model4)










