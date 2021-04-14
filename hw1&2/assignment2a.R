data <- read.csv(file.choose(), stringsAsFactor = F)
str(data)
summary(data)
data$De <- factor(data$De);levels(data$De)[2]<-"notadmit"
summary(data$De)
plot(data$GMAT, data$GPA, main="Scatter plot between GPA and GMAT", xlab="GMAT")
x <- aggregate(data$GPA ~ data$De, data=data, FUN=mean)
summary(x)
str(x)
y <- aggregate(data$GMAT ~ data$De, data=data, FUN=mean)
str(y)
prop.table(table(data$De))*100
nRows <- nrow(data)
nRows
trainSize <- ceiling(nRows * 0.7)
trainSize
set.seed(1234)
train.index <- sample(1:nRows, trainSize, replace = F)
train.index[1:10]
data.train <- data[train.index,]
str(data.train)
str(train.index[1:10])
summary(data.train)

library(knitr)
aggregate(data$GPA ~ data$De, FUN=mean)
aggregate(data$GMAT ~ data$De, FUN=mean)
str(data)
aggregate(data.train$GPA, FUN=mean)
minmax <- function(x){
  ifelse(is.numeric(x),
         return( ( x - min(x) ) / ( max(x) - min(x) ) ),
         return (x) )
}
minmax(data.train$GPA)
minmax(data.train$GMAT)
summary(minmax(data.train$GPA))
summary(data.train$GPA)

nRows <- nrow(data)
nRows
ts <- floor(nRows*0.6)
ts
set.seed(4567)
train <- sample(1:nRows, ts, replace = F)
train[1:10]
data.train <- data[train,]
str(data.train)
str(train[1:10])
data.test <- data[-train,]
str(data.test)

library(class)
library(gmodels)

predicted.out <- knn(data.train[-3], data.test[-3], data.train$De, k=5)
CrossTable(data.test$De, predicted.out, dnn=c("Actual", "Predicted"))

str(data)
summary(data)
data$De <- factor(data$De);levels(data$De)[2]<-"notadmit"
summary(data$De)
data$GPA <- scale(data$GPA)
summary(data$GPA)
nRows <- nrow(data)
nRows
trainSample <- floor(nRows*0.6)
trainSample
set.seed(4567)
train <- sample(1:nRows, trainSample, replace = F)
train[1:10]
data.train <- data[train,]
str(data.train)
str(train[1:10])
data.test <- data[-train,]
str(data.test)
predicted.out <- knn(data.train[-3], data.test[-3], data.train$De, k=5)
CrossTable(data.test$De, predicted.out, dnn=c("Actual", "Predicted"))

library(e1071)
model <- naiveBayes(data.train[-3], data.train$De, laplace=1)
predicted.test.bayes <- predict(model, data.test[-3])
CrossTable(data.test$De, predicted.test.bayes, dnn=c("Actual", "Predicted"))
