data <- read.csv(file.choose(), stringsAsFactor = F)
str(data)
sched <- floor(data$schedtime / 100)
table(sched)
data$dayweek <- factor(data$dayweek)
str(data$dayweek)
levels(data$dayweek) <- c("0", "0", "0", "0", "0", "1", "1")
summary(data$dayweek)

#back up data
data.org <- data
data <- data.org

#removed unused variables starting from the last variable
data <- data[-12]
data <- data[-11]
data <- data[-7]
data <- data[-6]
data <- data[-5]
data <- data[-3]
data <- data[-1]

table(data$delay, data$weather)
table(data$origin)
table(data$dest)
table(data$origin, data$delay)
prop.table(table(data$origin, data$delay)*2201)*100
table(data$carrier)

data.save <- data

#recoded origin
data$origin <- factor(data$origin)
str(data$origin)
levels(data$origin) <- c("0", "1", "0")

#recoded destination
data$dest <- factor(data$dest)
str(data$dest)
levels(data$dest) <- c("0", "0", "1")

#recoded carrier
data$carrier <- factor(data$carrier)
str(data$carrier)
levels(data$carrier) <- c("0", "1", "0", "0", "0", "0", "0", "0")

#normalization
minmax <- function(x){
  ifelse(is.numeric(x),
         return( ( x - min(x) ) / ( max(x) - min(x) ) ),
         return (x) )
}
data.backup <- data

data <- data.frame(lapply(data, minmax))
str(data)

# creating training and testing samples
nRows <- nrow(data)
nRows
trainSize <- floor(nRows * 0.6)
trainSize
set.seed(1234)
train <- sample(1:nRows, trainSize, replace = F)
train[1:10]
data.train <- data[train,]
data.test <- data[-train,]
str(data.test)
str(data.train)

#knn model
library(class)
predicted <- knn(data.train[-6], data.train[-6], data.train$delay, k = 10)
predicted[1:10]
library(gmodels)
CrossTable(data.train$delay, predicted, dnn=c("Actual", "Predicted"))

#using test sample to knn model
library(class)
predicted.out <- knn(data.train[-6], data.test[-6], data.train$delay, k = 10)
predicted.out[1:10]
library(gmodels)
CrossTable(data.test$delay, predicted.out, dnn = c("Actual", "Predicted"))

#naive bayesian
library(e1071)
model <- naiveBayes(data.train[-6], data.train$delay, laplace=1)
predicted.test.bayes <- predict(model, data.test[-6])
CrossTable(data.test$delay, predicted.test.bayes, dnn=c("Actual", "Predicted"))
