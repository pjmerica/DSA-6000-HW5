library(dplyr)
library(tidyverse)
#flight <-  read.csv(file="flight.csv", header=T)
flight <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
head(flight)
#flight1 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
#head(flight1)

#backup
flight_bkup <- flight
#what does the data look like
summary(flight)

#summary(flight1)
#summary(flight_bkup)
#testset <- anti_join(flight1,flight_bkup)
#how many months are analyzed
u<-unique(flight$Month)

#comparedf(flight,flight1)
#summary(comparedf(flight,flight1))

#Data cleaning

#Remove NAs
which(is.na(flight))
which(! complete.cases(flight))
flight <- flight[complete.cases(flight), ] 
#remove duplicates
flight <- distinct(flight)

#data observations
#distance negative and 0 - looks like high leverage point
#schld elapse time of 0 and negative looks like high leverage points
#arr delay and depdeplay are 0 for cancelled flights


#Analyzing the cancellation ratio
table(flight$Canceled)
barplot(prop.table(table(flight$Canceled)) * 100, main = "Cancellation",xlab = "cancelled",  
        ylab = "# flights")
#~80 of flights are not cancelled.


# check for high leverage points

all_columns <- colnames(flight)
all_formula_strings <- paste0(all_columns," ~ Canceled")
#par(mfrow = c(1, 4))
for (i in c(2,3,5:8)) {
  boxplot(as.formula(all_formula_strings[i]), data = flight,
          col = rgb(1, 0, 0, alpha = 0.4), 
          xlab = "Canceled", 
          ylab = all_columns[i], 
          main = paste0("Box plot for \n", all_columns[i]),
          border = "black", 
          outpch = 25,      
          outbg = "green",  
          whiskcol = "blue",
          whisklty = 2,    
          lty = 1)
}
#par(mfrow = c(1, 1))
#remove high leverage points
flight[flight$Distance <= 0,]
flight <- flight[!flight$Distance <= 0,]
flight[flight$Distance >10000,]
flight <- flight[!flight$Distance >10000,]
flight[flight$SchedElapsedTime <=0 ,]
flight <- flight[!flight$SchedElapsedTime <=0 ,]


set.seed(10)  # <-- group 10
trainset <- sample(1:nrow(flight), 1000)  # DO NOT CHANGE: you must sample 80 data points for training
validset <- setdiff(1:nrow(flight), trainset) 
testset<- flight[validset,]
m0 <- glm (Canceled ~ 1, data = flight[trainset,] , family = binomial )
summary(m0)
pred0 <- predict(m0,testset,type =  "response")
prob0 <- ifelse (pred0 > 0.16, 1,0)
table(prob0, testset$Canceled)
dff <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                  m1 = pred0)
AUCm0 <- ROC_func(dff, 1, 2, add_on = F, color = 1)
cat(paste0("Model  AUC is ", ": ", format(AUCm0, digits = 5), "\n"))

m1 <- glm (Canceled ~ .-ArrDelay , data = flight[trainset,] , family = binomial )
summary(m1)
pred1 <- predict(m1,testset,type =  "response")
prob1 <- ifelse (pred1 > 0.23, 1,0)
table(prob1, testset$Canceled)
dff <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                  m1 = pred1)
AUCm1 <- ROC_func(dff, 1, 2, add_on = F, color = 1)
cat(paste0("Model  AUC is ", ": ", format(AUCm1, digits = 5), "\n"))


m2 <- glm (Canceled ~ .-ArrDelay - DepDelay , data = flight[trainset,] , family = binomial )
summary(m2)
pred2 <- predict(m2,testset,type =  "response")
prob2 <- ifelse (pred2 > 0.23, 1,0)
table(prob2, testset$Canceled)
dff <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                  m1 = pred2)
AUCm2 <- ROC_func(dff, 1, 2, add_on = F, color = 1)
cat(paste0("Model  AUC is ", ": ", format(AUCm2, digits = 5), "\n"))

m3 <- glm (Canceled ~ .-ArrDelay - DepDelay - DepartureTime , data = flight[trainset,] , family = binomial )
summary(m3)
pred3 <- predict(m3,testset,type =  "response")
prob3 <- ifelse (pred3 > 0.23, 1,0)
table(prob3, testset$Canceled)
dff <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                  m1 = pred3)
AUCm3 <- ROC_func(dff, 1, 2, add_on = F, color = 1)
cat(paste0("Model  AUC is ", ": ", format(AUCm3, digits = 5), "\n"))

library(randomForest)
mrf <- randomForest (Canceled ~ .-ArrDelay - DepDelay , data = flight[trainset,] , family = binomial )
summary(mrf)
predrf <- predict(mrf,testset,type =  "response")
#prob2 <- ifelse (pred2 > 0.2, 1,0)
#table(prob2, testset$Canceled)
df <- data.frame(label  = ifelse(testset$Canceled == "1", 1, 0),
                 m1 = predrf)
AUCrf <- ROC_func(df, 1, 2, add_on = F, color = 1)
cat(paste0("Model  AUC is ", ": ", format(AUCrf, digits = 5), "\n"))





