#Input Data
#home<-setwd(Sys.getenv("HOME"))
#fpath<-file.path(home,"../git/DataMining/Assignment2","haberman.data")
#habermansurvival <- read.csv(fpath, header = FALSE, sep = ",", stringsAsFactors = TRUE, col.names = c("age", "operation_year", "positive_axillary_nodes", "survival_status"), na.strings = "?")

library(nnet)
library(rpart)

data(iris)
#str(iris)

set.seed(2345)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7,0.3))

trainData <- iris[ind==1,]
testData <- iris[ind==2,]

nn1 <- nnet(Species~., trainData, size = 0, skip=TRUE, linout=TRUE)
nn2 <- nnet(Species~., trainData, size=2, rang=0.1, decay=5e-4, maxit=200) 

table(testData$Species,predict(nn1, testData, type = "class"))
table(testData$Species,predict(nn2, testData, type = "class"))

library(e1071)
library(MASS)

data(cats)
model <- svm(Sex~., data = cats)
print(model)
summary(model)

plot(model,cats)

set.seed(1234)
ind <- sample(2, nrow(cats), replace=TRUE, prob=c(0.7,0.3))
trainData <- cats[ind==1,]
testData <- cats[ind==2,]

model <- svm(Sex~., data = trainData)
prediction <- predict(model, testData[,-1])

tab <- table(pred = prediction, true = testData[,1])

classAgreement(tab)

plot(model,trainData)  
plot(model,trainData)  

tuned <- tune.svm(Sex~., data = trainData, gamma = 10^(-6:-1), cost = 10^(1:2)) 
summary(tuned)


#Bagging
library(adabag)
library(mlbench)

data(Vehicle)
l <- length(Vehicle[,1])

sub <- sample(1:l,2*l/3)

Vehicle.bagging <- bagging(Class ~.,data=Vehicle[sub, ], mfinal=15, control = rpart.control(maxdepth=5, minsplit=15))

Vehicle.bagging.pred <- predict.bagging(Vehicle.bagging,newdata=Vehicle[-sub, ], newmfinal=10)

Vehicle.bagging.pred$confusion
Vehicle.bagging.pred$error


#Boosting
Vehicle.adaboost <- boosting(Class ~.,data=Vehicle[sub, ],mfinal=15, coeflearn="Zhu", control=rpart.control(maxdepth=5, minsplit=15))

Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost,newdata=Vehicle[-sub, ])

Vehicle.adaboost.pred$confusion

Vehicle.adaboost.pred$error 
