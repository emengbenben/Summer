## Ben Wang
## JHU INBT Lab Patient Ambulation Predictive Modeling Study
## 2019 Summer

# load pacakges
library(randomForest)
library(pdp)
library(rpart)
library(neuralnet)
library(rfUtilities)
library(DMwR)
library(ggplot2)
library(caret)
library(ISLR)
library(leaps)
library(bestglm)
library(caret)
library(party)

## load data from local csv file
readmissionData <- read.csv("C:\\Users\\wangb\\Desktop\\Prediction\\Prediction\\Ben_Data\\30days_Redcap_deleted_two_days2.csv", fileEncoding="UTF-8-BOM",
               header =TRUE)

y <- read.csv("C:\\Users\\wangb\\Desktop\\Prediction\\Prediction\\Ben_Data\\30days_Redcap_deleted_two_days3.csv", fileEncoding="UTF-8-BOM",
                            header =TRUE)

Data1 <- read.csv("C:\\Users\\wangb\\Desktop\\Prediction\\Prediction\\Ben_Data\\30days_Redcap_deleted_two_days5.csv", fileEncoding="UTF-8-BOM",
                            header =TRUE)

Data2 <- read.csv("C:\\Users\\wangb\\Desktop\\Prediction\\Prediction\\Ben_Data\\30days_Redcap_deleted_two_days4.csv", fileEncoding="UTF-8-BOM",
                  header =TRUE)

trainRowCount <- 77

TP <- 0
TN <- 0
FP <- 0
FN <- 0


for(i in 1:100){
  #generate 24 patients(12->readmin 12->notreadmin)
  set.seed(i)
  trainIndex <- sample(nrow(Data1), trainRowCount)
  train <- Data1[trainIndex,]
  train <- rbind(train,Data2)
  
  #select one for testing others for training.
  trainIndex1 <- sample(nrow(train), 148)
  train1 <- train[trainIndex1,]
  test <- train[-trainIndex1,]
  y_test <- test$X30.day.readmission
  
  rf <- randomForest(as.factor(X30.day.readmission) ~ Ambulations + Ambulation_Day1 + Ambulation_Day2 + Ambulation_Day3 + Duration + 
                       Duration_Day1 + Duration_Day2 + Duration_Day3 + Average.Speed + Average.Speed1 + Average.Speed2 + Average.Speed3 + Speed_max + Speed_min + Total.Distance.Walked+ Distance_Day1 + Distance_Day2 + Distance_Day3
                     + Hours.Since.Last.Walk_Day1 + Hours.Since.Last.Walk_Day2 + Hours.Since.Last.Walk_Day3, #+ speed_change1 + speed_change2,
                     data = train1, ntree = 100, importance=TRUE, probability = TRUE)
                  
  rf
  predictions <- predict(rf, test, type="response")

  result <-mean(as.numeric(as.character(predictions)))
  
  if(y_test == result){
    if(y_test == 0){
      TP = TP + 1 
    }
    else{
      TN = TN + 1
      print(predictions)
    }
  }
  else{
    if(y_test == 0){
      FP = FP + 1
    }
    else{
      FN = FN + 1
    }
  }
}

TP

TN

FP

FN



rf <- randomForest(as.factor(X30.day.readmission) ~ Ambulations + Ambulation_Day1 + Ambulation_Day2 + Ambulation_Day3 + Duration + 
                     Duration_Day1 + Duration_Day2 + Duration_Day3 + Average.Speed + Average.Speed1 + Average.Speed2 + Average.Speed3 + Total.Distance.Walked+ Distance_Day1 + Distance_Day2 + Distance_Day3,
                   data = readmissionData, ntree = 100, importance=TRUE, probability = TRUE)

rf

predictions <- predict(rf, readmissionData, type="response")
predictions


predictions <- predict(rf, readmissionData, type="prob")

predictions


for(i in 1:91){
  if(predictions[i] == "1"){
    print(predictions[i])
  }
}



# Neural Network. 
NN <- neuralnet(as.factor(X30.day.readmission)  ~ ., readmissionData, hidden = 3 , linear.output=FALSE)
NN$result.matrix
# plot neural network
plot(NN)
prediction <- predict(NN,readmissionData , type="response")
prediction

temp_test <- subset(test, select = c("length_of_stay.days.","Ambulations", "Duration", "Average.Speed", "Total.Distance.Walked"))


varImpPlot(rf,type=2)
par <- partial(rf, pred.var = "X30.day.readmission", prob = "true")
plot <- autoplot(par, contour = TRUE)

```

