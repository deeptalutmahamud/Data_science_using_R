
mydata <- read.csv("G:/Lectures/Semester 10/DS/Mall_Customers.csv")
mydata


install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)


str(mydata)
sum(is.na(mydata))
head(mydata)
summary(mydata)


data<- select(mydata, c(1,2,3,4))
data

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
}

wssplot(data)

km = kmeans(data,2)

autoplot(km,data,frame=TRUE)

km$centers
