# 1.	Import dataset to R using stringsAsFactors = FALSE. 
# Run the following transformations:
#  dt[c(2,3,6,7,9,13,14)]<-data.frame(lapply(dt[c(2,3,6,7,9,13,14)],factor))
# dt <- dt[c(2,3,9,10,11,12,14)]
dt <- read.csv(file.choose(), stringsAsFactors = FALSE)
dt[c(2,3,6,7,9,13,14)]<-data.frame(lapply(dt[c(2,3,6,7,9,13,14)],factor))
dt <- dt[c(2,3,9,10,11,12,14)]


# 2.	Pick 70% records randomly as training dataset. 
set.seed(1234)
dt <- sample(dt)
str(dt)
nrows <- nrow(dt)
train.size <- floor(nrows * 0.7)
dt.train <- dt[1:train.size,]
dt.test <- dt[-c(1:train.size),]


# 3.	Build a logit regression model where statlogheart is the target variable. 
dt.model <- glm(statlogheart ~ ., data = dt.train, family=binomial)
summary(dt.model)


# 4.	What is the McFadden R^2 for this model? Evaluate the model by its fitness.
null.model <- glm(statlogheart ~ 1., data = dt.train, family="binomial")
1-logLik(dt.model)/logLik(null.model)
1-(141.97/260.10)
## two different ways to get McFadden R^2
## we see a 45% fit in this logit regression model


# 5.	Evaluate the performance of training dataset by using cross table.  What percentage of people who do not have heart disease are wrongly predicted?
#  Hint for predict: predicted<-predict(model, dt.train, type= "response")
# Hint for converting logs ratio: predicted<- ifelse(predicted>=0.5,2,1)
predicted.train.logit <- predict(dt.model, dt.train)
head(predicted.train.logit)
head(exp(predicted.train.logit)/(1+exp(predicted.train.logit)))
predicted.train.logit <- predict(dt.model, dt.train, type='response')
head(predicted.train.logit)
predicted.train.logit <- ifelse(predicted.train.logit <=0.5, 1, 2)
library(gmodels)
CrossTable(dt.train$statlogheart, predicted.train.logit, dnn = c("actual", "predicted"))
# What percentage of people who do not have heart disease are wrongly predicted?
# 9.6% were predicted to have heart disease but they actually don't have it. These are the False Positives in the top right corner of the cross table

# 6.	Evaluate the performance of testing dataset by using cross table.  What percentage of people who have heart disease are wrongly predicted?
#  Hint for predict: predicted<-predict(model, dt.test, type= "response")
# Following returns log of odds ratio and not probabilities
# 1=Absence of heart disease; 2= presence of heart disease;
predicted.test.logit <- predict(dt.model, dt.test)
head(predicted.test.logit)
head(exp(predicted.test.logit)/(1+exp(predicted.test.logit)))
predicted.test.logit <- predict(dt.model, dt.test, type='response')
head(predicted.test.logit)
predicted.test.logit <- ifelse(predicted.test.logit <=0.5, 1, 2)
library(gmodels)
CrossTable(dt.test$statlogheart, predicted.test.logit, dnn = c("actual", "predicted"))
# What percentage of people who have heart disease are wrongly predicted?
# 28.6% predicted to not have it but they actually do have heart disease.These are False Negatives in the bottom left corner of the cross table.

# 7.	Predict using naiveBayes
# a.	Predict classification for the training dataset and create cross table.
# Note for predict: parameter type=response is needed only for logit. 
# Predict classification for the testing dataset and create cross table. 
library(e1071)
model.naive <- naiveBayes(statlogheart ~ ., dt.train, laplace=1)
predicted.train.naive <- predict(model.naive, dt.train[-4])
CrossTable(dt.train$statlogheart, predicted.train.naive, dnn=c('actual', 'predicted'))

model.naive <- naiveBayes(statlogheart ~ ., dt.train, laplace=1)
predicted.test.naive <- predict(model.naive, dt.test[-4])
CrossTable(dt.test$statlogheart, predicted.test.naive, dnn=c('actual', 'predicted'))

# 8.	Predict using decision tree
# a.	Predict classification for the training dataset and create cross table.
# Predict classification for the testing dataset and create cross table. 
library(C50)
model.tree <- C5.0(statlogheart ~ ., dt.train, trials=1)
predicted.train.tree <- predict(model.tree, dt.train[-4])
CrossTable(dt.train$statlogheart, predicted.train.tree, dnn=c('actual', 'predicted'))


model.tree <- C5.0(statlogheart ~ ., dt.train, trials=1)
predicted.test.tree <- predict(model.tree, dt.test[-4])
CrossTable(dt.test$statlogheart, predicted.test.tree, dnn=c('actual', 'predicted'))



# 9.	Predict using knn at k=3
# a.	Predict classification for the training dataset and create cross table. 
# Hint: train=dt.train, test=dt.train
# b.	 Predict classification for the testing dataset and create cross table. 
# Hint: train=dt.train, test=dt.test
library(class)
str(dt.train)
predicted.train.knn <- knn(dt.train[-4], dt.train[-4], dt.train[[4]], k=3)
CrossTable(dt.train$statlogheart, predicted.train.knn, dnn=c("actual", "predicted"))

str(dt.test)
predicted.test.knn <- knn(dt.train[-4], dt.test[-4], dt.train[[4]], k=3)
CrossTable(dt.test$statlogheart, predicted.test.knn, dnn=c("actual", "predicted"))



# 10.	Compare performance of models using training dataset
#Accuracy = (TP + TN) / (TP + TN + FP + FN) = % ACCURACY
CrossTable(dt.train$statlogheart, predicted.train.logit, dnn=c("actual", "predicted"))
#Accuarcy of logit = (68+94)/(189) = 85.71% accuracy
CrossTable(dt.train$statlogheart, predicted.train.naive, dnn=c('actual', 'predicted'))
#Accuarcy of naive = (91+64)/(189) = 82.01% accuracy
CrossTable(dt.train$statlogheart, predicted.train.tree, dnn=c('actual', 'predicted'))
#Accuarcy of tree = (96+69)/(189) = 87.3% accuracy
CrossTable(dt.train$statlogheart, predicted.train.knn, dnn=c("actual", "predicted"))
#Accuracy of knn = (97+67) / (189) = 86.77% accuracy

# 11.	Compare performance of models using testing dataset
CrossTable(dt.test$statlogheart, predicted.test.logit, dnn = c("actual", "predicted"))
#Accuarcy of logit = (43+25)/(81) = 83.95% accuracy
CrossTable(dt.test$statlogheart, predicted.test.naive, dnn=c('actual', 'predicted'))
#Accuarcy of naive = (42+26)/(81) = 83.95% accuracy
CrossTable(dt.test$statlogheart, predicted.test.tree, dnn=c('actual', 'predicted'))
#Accuarcy of tree = (43+26)/(81) = 85.19% accuracy
CrossTable(dt.test$statlogheart, predicted.test.knn, dnn=c("actual", "predicted"))
#Accuarcy of knn = (39+28)/(81) = 70.37% accuracy


# 12.	Which model would you recommend overall? 
#  Hint: Typically you want to give preference to a model with good test dataset performance. If 2 models perform equally well over test dataset then look for model that performs well for training dataset. 
# If you are looking for attribution and not prediction then you try to get the performance of regression model as close to the best performing other model, and then use regression model. 

#Decision trees have the highest accuracy when using predictions in both training and testing sets 
#therefore, I believe using decision trees would be recommended overall.



