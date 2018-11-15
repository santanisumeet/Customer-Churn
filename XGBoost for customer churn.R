
getwd()


# Importing the dataset
dataset = read.csv('cust.csv')
dataset = dataset[-c(1)]


#one hot encoding

library(caret)


dmy <- dummyVars(" ~ .", data = dataset)
trsf <- data.frame(predict(dmy, newdata = dataset))


dataset = trsf[-c(58, 59)]


extra = read.csv('cust.csv')


dataset$Churn = extra$Churn


#recoding the dependent variable
A <- ifelse(dataset[,58] == "No", 0, ifelse(dataset[,58] == "Yes", 1, 99))

dataset$Churn = A




library(caTools)
set.seed(123)
split = sample.split(dataset$Churn, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#Fitting XGBoost to training set

install.packages("xgboost")
library(xgboost)


classifier = xgboost(data = as.matrix(training_set[-11]), label = training_set$Churn, nrounds = 32)




# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$Churn, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-58]), label = training_set$Churn, nrounds = 32)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-58]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 58], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

accuracy = mean(as.numeric(cv))
accuracy


