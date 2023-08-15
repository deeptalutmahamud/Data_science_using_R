# Install and load the 'impute' package
install.packages("imputeTS")
library(imputeTS)

 

install.packages("VIM")
library(VIM)

 

install.packages("zoo")
library(zoo)

 

install.packages("dplyr")
install.packages("ggplot2")
install.packages("rpart")
install.packages("caret")
install.packages("lattice")
install.packages("e1071")

 

library(dplyr)
library(ggplot2)
library(lattice)
library(rpart)
library(rpart.plot)
library(caret)  # For k-fold cross-validation
library(e1071)  # For confusion matrix

 

setwd("E:/Sem-11/Data mining/Project")

 

# load the data
alldata <- read.csv("Diabetes.csv")

 

 

head(alldata)      # View the first few rows of the data
summary(alldata)   # Summary statistics of the data
str(alldata)       # Structure of the data

 

View(alldata)

 

 

#write.csv(alldata, "Diabetes.csv", row.names = FALSE)

 

 

# Replace 0 values with NA
#alldata[alldata == 0] <- NA

 

# Replace 0 with NA in all columns except 'Outcome'
alldata <- alldata %>%
  mutate(across(-Outcome, ~ ifelse(. == 0, NA, .)))

 

 

# Identify columns with NA values
columns_with_na <- colSums(is.na(alldata)) > 0

 

# Print the column names with NA values
print(names(columns_with_na)[columns_with_na])

 

 

# Calculate the median of the BloodPressure column
median_BP <- median(alldata$BloodPressure)

 

# Calculate the median of the Glucose column
median_G <- median(alldata$Glucose)

 

# Calculate the median of the SkinThickness column
median_Skin <- median(alldata$SkinThickness)

 

# Calculate the median of the BMI column
median_G <- median(alldata$BMI)

 

 

# Replace NA values with the median of BloodPressure
alldata$BloodPressure <- na.aggregate(alldata$BloodPressure, FUN = median)

 

# Replace NA values with the median of Glucose
alldata$Glucose <- na.aggregate(alldata$Glucose, FUN = median)

 

# Replace NA values with the median of SkinThickness
alldata$SkinThickness <- na.aggregate(alldata$SkinThickness, FUN = median)

 

# Replace NA values with the median of BMI
alldata$BMI <- na.aggregate(alldata$BMI, FUN = median)

 

# Find the minimum and maximum age
min_age <- min(alldata$Age)
max_age <- max(alldata$Age)

 

# Print the minimum and maximum age
print(min_age)
print(max_age)

 

# Set the desired age range
min_age <- 21
max_age <- 81

 

 

# Find the median insulin value for the desired age range
median_insulin <- median(alldata$Insulin[alldata$Age >= min_age & alldata$Age <= max_age], na.rm = TRUE)

 

# Replace NA values in "Insulin" column with the median insulin value
alldata$Insulin <- ifelse(is.na(alldata$Insulin) & alldata$Age >= min_age & alldata$Age <= max_age, median_insulin, alldata$Insulin)

 

 

# View the modified dataset
print(alldata)
View(alldata)

 

write.csv(alldata, "Diabetes.csv", row.names = FALSE)

 

 

# Define the target variable
target_col <- "Outcome"

 

# Define the independent variables
independent_cols <- setdiff(names(alldata), target_col)

 

# Build decision trees with different attribute selection methods
methods <- c("ent", "gain", "gini")
trees <- list()

 

for (method in methods) {
  formula <- as.formula(paste(target_col, "~ ."))
  tree <- rpart(formula, data = alldata, parms = list(split = method))
  trees[[method]] <- tree
}

 

# K-fold cross-validation
k_fold <- c(5, 10)

 

for (k in k_fold) {
  set.seed(123)  # For reproducibility
  ctrl <- trainControl(method = "cv", number = k)

  for (method in methods) {
    model <- train(as.formula(paste(target_col, "~ .")), data = alldata,
                   method = "rpart", trControl = ctrl,
                   parms = list(split = method))

    # Make predictions
    predictions <- predict(model, alldata)

    # Create confusion matrix
    confusion <- confusionMatrix(predictions, alldata$Outcome)

    print(paste("Method:", method, "| K-Fold:", k))
    print(confusion)
  }
}
View(alldata)
update.packages(ask = FALSE)