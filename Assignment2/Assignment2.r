#Input Data
home<-setwd(Sys.getenv("HOME"))
fpath<-file.path(home,"../git/DataMining/Assignment2","haberman.data")
habermansurvival <- read.csv(fpath, header = FALSE, sep = ",", stringsAsFactors = TRUE, col.names = c("age", "operation_year", "positive_axillary_nodes", "survival_status"), na.strings = "?")
habermansurvival[1:30,]


# Summarize Class Output
sum.class <- summary(habermansurvival$survival_status)
sum.class


# Summarize age
sum.age <- summary(habermansurvival$age)
sum.age


# Summarize operation year
sum.operation_year <- summary(habermansurvival$operation_year)
sum.operation_year


# Summarize age
sum.positive_axillary_nodes <- summary(habermansurvival$positive_axillary_nodes)
sum.positive_axillary_nodes


# Decision Tree Classification
set.seed(1234)
ind <- sample(2, nrow(habermansurvival), replace=TRUE, prob=c(0.7,0.3))

habermansurvival['survival_status'] <- as.factor(habermansurvival[, 'survival_status'])

trainData <- habermansurvival[ind==1,]
testData <- habermansurvival[ind==2,]

habermansurvival_rpart <- rpart(survival_status ~ age + operation_year + positive_axillary_nodes, data = trainData, method = "class")

printcp(habermansurvival_rpart)
plotcp(habermansurvival_rpart)
plot(habermansurvival_rpart)
text(habermansurvival_rpart, use.n=TRUE)

habermansurvival_pred <- predict(habermansurvival_rpart, newdata = testData)


#Random Forest
fit<-randomForest(survival_status ~ age + operation_year + positive_axillary_nodes, data = trainData, method = "class")
print(fit)
importance(fit)


# Naïve Bayes Classification

classifier <- naiveBayes(survival_status ~ age + operation_year + positive_axillary_nodes, data = trainData, method = "class")
print(classifier)
pred <- predict(classifier, testData)
pred
table(pred,testData$survival_status)