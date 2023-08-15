[03/07 20:35] RAHATUL AJEM KHAN



 # Importing necessary libraries 

library(corrplot) 

library(caret) 

library(dplyr) 

library(class) 

library(ggplot2) 


# Setting the working directory to "~/datasets/cancer_dataset" 

setwd("~/datasets/cancer_dataset") 


# Reading the data from the CSV file located at "~/datasets/cancer_dataset/cancer_prediction.csv" 

data <- read.csv("~/datasets/cancer_dataset/cancer_prediction.csv") 


# Printing the contents of the data to the console 

print(data) 


# Checking the dimensions of the dataset (number of rows and columns) 

dim(data) 


# Listing the column names of the dataset 

names(data) 


# Displaying the structure of the dataset (data types and column details) 

str(data) 


# Providing a summary of the dataset (statistical summary for each column) 

summary(data) 


# Creating a vector of column names to drop from the dataset 

drops <- c('Patient.Id', 'index') 


# Removing the specified columns from the dataset 

data <- data[ , !(names(data) %in% drops)] 


# Displaying the structure of the modified dataset (without the dropped columns) 

str(data) 


# Checking the column-wise count of missing null values in the dataset 

colSums(is.na(data)) 


# Creating a copy of the dataset 

data_copy <- data 


# Displaying the structure of the copied dataset 

str(data_copy) 


# Creating a vector of column names to drop from the copied dataset 

level_drops <- c('Level') 


# Removing the specified columns from the copied dataset 

data_copy <- data[ , !(names(data) %in% level_drops)] 


# Displaying the structure of the modified copied dataset 

str(data_copy) 


# Defining a function to normalize data within a column 

normalize <- function(x) { 

 return ((x - min(x)) / (max(x) - min(x))) 

} 


# Applying the normalization function to all columns except the last one (column 24) 

norm_data <- as.data.frame(lapply(data[,-24], normalize)) 


# Displaying the structure of the normalized dataset 

str(norm_data) 


# Providing a summary of the normalized dataset 

summary(norm_data) 


# Setting a random seed for reproducibility 

set.seed(123) 


# Creating an index vector for the training dataset 

data.d <- sample(1:nrow(norm_data), size = nrow(norm_data) * 0.8, replace = FALSE) 


# Splitting the dataset into training and testing datasets using the index vector 

train_data <- norm_data[data.d, ] 

test_data <- norm_data[-data.d, ] 


# Creating labels for the training and testing datasets 

train_labels <- data$Diagnosis[data.d] 

test_labels <- data$Diagnosis[-data.d] 


# Calculating an optimal value for k for the k-nearest neighbors algorithm 

opt_k <- round(sqrt(nrow(train_data))) 


# Checking if opt_k is even; if so, increment it by 1 (k should be odd for knn) 

k1 <- opt_k # First value of k 

k2 <- opt_k + 2 # Second value of k 


# Function to perform k-nearest neighbors classification with specified distance measure 

knn_classification <- function(train_data, test_data, train_labels, k, distance_measure) { 

 if ((k %% 2) == 0) { 

 k <- k + 1 

 } 

 return(knn(train_data, test_data, train_labels, k, prob = TRUE, use.all = TRUE, method = distance_measure)) 

} 






[03/07 20:40] RAHATUL AJEM KHAN



# Performing k-nearest neighbors classification with Euclidean Distance (k1) 

cancer_prediction_k1 <- knn_classification(train_data, test_data, train_labels, k1, "euclidean") 


# Performing k-nearest neighbors classification with Manhattan Distance (k2) 

cancer_prediction_k2 <- knn_classification(train_data, test_data, train_labels, k2, "manhattan") 


# Performing k-nearest neighbors classification with Minkowski Distance (k2) 

cancer_prediction_k3 <- knn_classification(train_data, test_data, train_labels, k2, "minkowski") 


# Converting the predicted labels to dataframes 

prediction_result_k1 <- as.data.frame(cancer_prediction_k1) 

prediction_result_k2 <- as.data.frame(cancer_prediction_k2) 

prediction_result_k3 <- as.data.frame(cancer_prediction_k3) 


# Comparing the predictions for the different values of k and distance measures 

comparison <- data.frame( 

 Test_Labels = test_labels, 

 K1_Euclidean_Prediction = cancer_prediction_k1, 

 K2_Manhattan_Prediction = cancer_prediction_k2, 

 K2_Minkowski_Prediction = cancer_prediction_k3 

) 


# Printing the comparison 

print(comparison) 


# Creating a correlation plot for the normalized dataset using the corrplot library 

corrplot(cor(norm_data), 

 method = "shade", 

 type = "full", 

 diag = TRUE, 

 tl.col = "black", 

 bg = "white", 

 title = "", 

 col = NULL) 


# Creating a histogram for the "Age" variable 

ggplot(data, aes(x = Age)) + 

 geom_histogram(fill = "blue", color = "black") + 

 labs(x = "Age", y = "Frequency", title = "Histogram") + 

 theme_minimal() 

