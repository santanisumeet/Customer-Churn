
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


hey = read.csv('cust.csv')


hey = read.csv('Class_churn.csv')



counts <- table(hey$Class)
barplot(counts, main="Class Distribution", 
        xlab="Customer Churned?", col = "blue", ylab = "#of Customers",
        names = c("No", "Yes"))


install.packages("plotly")
library(plotly)

x = c("No", "Yes")
y = c(73, 27)

data <- data.frame(x, y)


p <- plot_ly(data, x = ~x, y = ~y, type = 'bar', 
             text = y, textposition = 'auto',
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Class Distribution(in%)",
         xaxis = list(title = "Customer Churned?"),
         yaxis = list(title = "Percentage"))

p







p <- plot_ly(data, x = ~x, y = ~y, type = 'bar', 
             text = y, textposition = 'auto',
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Class Distribution(in%)",
         xaxis = list(title = "Customer Churned?"),
         yaxis = list(title = "Percentage"))

p

j = subset(test_set, test_set$Churn == 1)
summary(j)


j = 1495
k = 5634-1495
k


(1495/5634)*100
k = 100 -j





#training set



x = c("No", "Yes")
y = c(73.46, 26.54)

data <- data.frame(x, y)


p <- plot_ly(data, x = ~x, y = ~y, type = 'bar', 
             text = y, textposition = 'auto',
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Class Distribution(in%)",
         xaxis = list(title = "Customer Churned?"),
         yaxis = list(title = "Percentage"))

p





