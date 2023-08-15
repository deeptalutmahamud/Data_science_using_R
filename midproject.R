#import csv file

data <- read.csv(file.choose(), header = TRUE, sep = ",")
print(data)

# Assume your dataset is stored in a variable called "data"
data <- na.omit(data)
print(data)

#find null element

sum(is.na(data$Age))
sum(is.na(data$weight.kg.))
sum(is.na(data$Delivery_number))
sum(is.na(data$Delivery_time))
sum(is.na(data$Caesarian))

#Mean

data$Age[is.na(data$Age)] <- mean(data$Age, na.rm= TRUE )
data$weight.kg.[is.na(data$weight.kg.)] <- mean(data$weight.kg., na.rm= TRUE)
data$Delivery_number[is.na(data$Delivery_number)] <- mean(data$Delivery_number, na.rm = TRUE )
data$Delivery_time[is.na(data$Delivery_time)] <- mean(data$Delivery_time, na.rm = TRUE )
data$Caesarian[is.na(data$Caesarian)] <- mean(data$Caesarian, na.rm = TRUE )
print(data)

#convert in integer

data$Age<- as.numeric(format(round(data$Age, 0)))
data$weight.kg.<- as.numeric(format(round(data$weight.kg., 0)))
data$Delivery_number<- as.numeric(format(round(data$Delivery_number, 0)))
data$Delivery_time<- as.numeric(format(round(data$Delivery_time, 0)))
data$Caesarian<- as.numeric(format(round(data$Caesarian, 0)))
print(data)

#Blood new collum,mean and integer
library(dplyr)
data<-data %>% 
  mutate(blood_pressure = case_when(
    Blood == "low" ~ 0,
    Blood == "normal" ~ 1,
    Blood == "high" ~ 2, 
  ),
  blood_pressure = ifelse(is.na(blood_pressure), mean(blood_pressure, na.rm = TRUE), blood_pressure)
  )
print(data)

data$blood_pressure<- as.numeric(format(round(data$blood_pressure, 0)))
print(data)

#new collum delivery status which depends on delivery time

library(dplyr)
data<-data %>% 
  mutate(Delivery_status = case_when(
    Delivery_time == 0 ~ "timely",
    Delivery_time == 1 ~ "premature",
    Delivery_time == 2 ~ "late", 
  ))
print(data)

#heart problems

library(dplyr)
data<-data %>% 
  mutate(Heart_problem = case_when(
    Heart == 0 ~ "apt",
    Heart == 1 ~ "inept",
  ))
print(data)

#patient emergency status
library(dplyr)
data<-data %>% 
  mutate(emergency = case_when(
    Heart == 0 & blood_pressure == 2 ~ "emergency",
    Heart == 1 & blood_pressure == 1 ~ "normal",
    Heart == 0 & blood_pressure == 1 ~ "normal",
    Heart == 1 & blood_pressure == 2 ~ "normal",
    blood_pressure == 0 ~ "normal",
  ))
print(data)

#cesar risk

library(dplyr)
data<-data %>% 
  mutate(cesar_risk = case_when(
    Caesarian == 1 & Delivery_time == 3 ~ "Danger",
    Caesarian == 0 & Delivery_time == 3 ~ "normal",
   Caesarian == 1 & Delivery_time == 2 ~ "Risk",
    Caesarian == 0 & Delivery_time == 2 ~ "normal",
   Caesarian == 0 & Delivery_time == 0 ~ "normal",
   Caesarian == 1 & Delivery_time == 0 ~ "normal",
    Delivery_time == 1 ~ "normal",
  ))
print(data)

#find mode
install.packages("DescTools")

library(DescTools)
mode <- Mode(data$Age)
print(mode)

library(DescTools)
mode <- Mode(data$weight.kg.)
print(mode)

