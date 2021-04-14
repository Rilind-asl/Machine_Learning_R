# 1.	Import the 'admit.csv' data file into R. Factorize the target variable admit and label the levels 0, 1 as 'Not admitted' and 'Admitted' respectively. Admitted is the positive event.
# 
data <- read.csv(file.choose(), stringsAsFactors = F)
data$admit <- factor(data$admit, levels=c("0", "1"), labels=c("Not Admitted", "Admitted"))
str(data$admit)

# 2.	Normalize the GPA, GMAT variables in the data frame.
#
normalize <- function(x){
  if(is.numeric(x))x<-(x-min(x))/(max(x)-min(x))
  return(x)
}

data <- data.frame(lapply(data, normalize))
str(data)

# 3.	Set the seed and split the dataframe in to 70% train and 30% test set.
#
rows <- nrow(data)
train.index <- sample(1:rows, floor(0.7*rows), replace=FALSE)
data.train <- data[train.index,]
data.test <- data[-train.index,]

# 4.	Use knn with k=5 to predict admit. 
# a.	Report out-of-sample total error rate of the model by using CrossTable. 
# b.	Create a ROC plot
# c.	Find AUC
#
library(class)
library(ROCR)
library(gmodels)
class.knn.test <- knn(data.train[-1],data.test[-1],data.train$admit,k=5,prob=TRUE)
prob.knn.test <- ifelse(class.knn.test=="Admitted",attr(class.knn.test,"prob"),1-attr(class.knn.test,"prob"))
head(prob.knn.test)
class.check <- ifelse(prob.knn.test>0.5,"Admitted", "Not Admitted")
table(class.knn.test,class.check)
CrossTable(data.test$admit, class.knn.test)
predObj.knn.test <- prediction(prob.knn.test,data.test$admit,label.ordering = c("Not Admitted", "Admitted"))
roc.knn.test <- performance(predObj.knn.test, "tpr", "fpr")
plot(roc.knn.test,main='ROC curve for KNN model', col="khaki", lwd=2)
abline(a=0,b=1,lwd=2,lty=2)
auc.knn.test <- performance(predObj.knn.test, "auc")
unlist(auc.knn.test@y.values)

# 5.	Use a naïve bayes model to predict admit. 
# a.	Report out-of-sample total error rate of the model by using CrossTable. 
# b.	Create a ROC plot
# c.	Find AUC
#
library(e1071)
model.nb <- naiveBayes(admit~., data=data.train, laplace = 1)
class.nb.test <- predict(model.nb, data.test)
CrossTable(data.test$admit,class.nb.test)
prob.nb.test <- predict(model.nb, data.test, type="raw")
head(prob.nb.test)
prob.nb.test <- prob.nb.test[,2]
class.check <- ifelse(prob.nb.test>0.5, "Admitted", "Not Admitted")
CrossTable(class.nb.test,class.check)
predObj.nb.test <- prediction(prob.nb.test, data.test$admit, label.ordering = c("Not Admitted", "Admitted"))
roc.nb.test <- performance(predObj.knn.test, "tpr", "fpr")
plot(roc.nb.test,main="ROC curve for Naive Bayes model", col="navy blue", lwd=2)
abline(a=0,b=1,lwd=2,lty=2)
auc.nb.test <- performance(predObj.nb.test, "auc")
unlist(auc.nb.test@y.values)


# 6.	Use a logit regression model to predict admit. 
# a.	Report out-of-sample total error rate of the model by using CrossTable. 
# b.	Create a ROC plot
# c.	Find AUC
#
model.logit <- glm(admit~., data = data.train, family= binomial)
summary(model.logit)
prob.logit.test <- predict(model.logit,data.test,type="response")
class.logit.test <- ifelse(prob.logit.test>0.5, "Admitted", "Not Admitted")
CrossTable(data.test$admit,class.logit.test)
predObj.logit.test <- prediction(prob.logit.test, data.test$admit, label.ordering = c("Not Admitted", "Admitted"))
roc.logit.test <- performance(predObj.logit.test, "tpr", "fpr")
plot(roc.logit.test, main="ROC curve for logit model", col="lavender", lwd=2)
abline(a=0,b=1,lwd=2,lty=2)
auc.logit.test <- performance(predObj.logit.test, "auc")
unlist(auc.logit.test@y.values)


# 7.	Use a SVM model to predict admit. 
# a.	Report out-of-sample total error rate of the model by using CrossTable. 
# b.	Create a ROC plot
# c.	Find AUC
#
library(kernlab)
model.svm <- ksvm(admit~.,data=data.train, kernel="rbfdot",prob.model=T)
class.svm.test <- predict(model.svm,data.test)
head(class.svm.test)
prob.svm.test <- predict(model.svm, data.test, type="probabilities")
head(prob.svm.test)
prob.svm.test <- prob.svm.test[,2]
class.check <- ifelse(prob.svm.test>0.5,"Admitted", "Not Admitted")
table(class.svm.test,class.check)
CrossTable(data.test$admit,class.svm.test)
class(data.test$admit)
predObj.svm.test <- ROCR::prediction(prob.svm.test,data.test$admit,label.ordering = c("Not Admitted", "Admitted"))
roc.svm.test <- performance(predObj.svm.test,"tpr","fpr")
plot(roc.svm.test,main="ROC curve for svm model", col="salmon", lwd=2)
abline(a=0,b=1,lwd=2,lty=2)
auc.svm.test <- performance(predObj.svm.test, "auc")
unlist(auc.svm.test@y.values)

# 8.	Use a neuralnet model to predict admit. Use 2 hidden neurons. 
# a.	Report out-of-sample performance of the model by using CrossTable. 
# b.	Create a ROC plot
# c.	Find AUC
#
library(neuralnet)
model.nn <- neuralnet(admit~., data=data.train, hidden=2)
computed.nn <- compute(model.nn, data.test)
head(computed.nn$net.result)
prob.nn.test<-computed.nn$net.result
head(prob.nn.test)
prob.nn.test<-(prob.nn.test[,1])
head(prob.nn.test)
class.nn.test <-ifelse(prob.nn.test>0.5,"Admitted", "Not Admitted")
CrossTable(data.test$admit, class.nn.test)
summary(prob.nn.test)
predObj.nn.test<- ROCR::prediction(prob.nn.test, data.test$admit, label.ordering=c("Not Admitted", "Admitted"))
roc.nn.test <- performance(predObj.nn.test, "tpr", "fpr")
plot(roc.nn.test,main="ROC curve for nn model", col="orange", lwd=2)
abline(a=0,b=1,lwd=2,lty=2)
auc.nn.test <- performance(predObj.nn.test, "auc")
unlist(auc.nn.test@y.values)

# 9.	Put ROC curves for all the above models together in a plot and suggest which one you think is the best performing model
#
plot(roc.nb.test, xlab="False Positive Rate", ylab="True Positive Rate", type='l', col="navy blue")
plot(roc.knn.test,col="turquoise", add=T)
plot(roc.logit.test,col="khaki", add=T)
plot(roc.svm.test,col="salmon", add=T)
plot(roc.nn.test,col="green", add=T)
abline(v=0,lty=2,lwd=1,col="gray60")
abline(h=1,lty=2,lwd=1,col="gray60")
abline(a=0,b=1,lty=2,lwd=1,col="gray60")
# I think the neuralnet is the best performing model

# 10.	Which model would you recommend based on CrossTable results? Which one would you recommend based on AUC?
#   
CrossTable(data.test$admit, class.knn.test)
CrossTable(class.nb.test,class.check)
CrossTable(data.test$admit,class.logit.test)
CrossTable(data.test$admit,class.svm.test)
CrossTable(data.test$admit, class.nn.test)
#knn model is recommended based on CrossTable results

unlist(auc.knn.test@y.values)
unlist(auc.nb.test@y.values)
unlist(auc.logit.test@y.values)
unlist(auc.svm.test@y.values)
unlist(auc.nn.test@y.values)
#Naive Bayes model is recommended based on AUC results

