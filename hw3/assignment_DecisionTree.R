#Q1 1.	Import data set to R and check the data 
# structure of the data set. 
data <- read.csv(file.choose(), stringsAsFactors = F)
str(data)

#Q2 2.	Change attributes 2,3,6,7,9,13, and 14 to factor variables. 
data[c(2,3,6,7,9,13,14)]<-data.frame(lapply(data[c(2,3,6,7,9,13,14)],factor))

#Q3 3.	Add labels (No disease, Heart disease) to statlogheart

data$statlogheart <- factor(data$statlogheart);levels(data$statlogheart)[2]<-"Heart Disease"
data$statlogheart <- factor(data$statlogheart);levels(data$statlogheart)[1]<-"No Disease"

#Q4 4.	Create a training dataset with 99% of records, 
# and use it for the rest of the analysis. 
# (Discussion: Typically, you will not want to 
# keep such a high percentage of records in training set. 
# I made this exception because results were not stable at 
# lower percentage split.)
install.packages("C50")
library(C50)
set.seed(1234)
totalNoOfRows <- nrow(data)
train.size <- floor(totalNoOfRows * 0.99)
train.indices <- sample(1:totalNoOfRows, train.size, replace = F)
str(train.indices)
data.train <- data[train.indices,]
str(data.train)
data.test <- data[-train.indices,]
#Q5 5.	Build a decision tree model with statlogheart 
# as outcome variable and the rest variables as predictors. 
# Then display simple facts about the tree. 
# What tree size does this decision tree model have?
m <- C5.0(data.train[-14], data.train$statlogheart, trials = 1, costs = NULL)
m

# Q6 6.	Summarize this decision tree model and 
# display the detailed information of the tree model. 
# How many patients have been misclassified? 
# How many attributes have been utilized to generate the tree. 
summary(m)

# Q7 7.	If a patient is female, 
# has one major vessel colored by flourosopy (majorvessels) and 
# has thal = 6, does this patient have heart disease base on 
# the decision tree?
summary(m)

# Q8 8.	If a patient has no major vessel colored, 
# angina is one, oldpeak is less than 1.5, has thal = 7, 
# serumchol is less than 255, 
# does this patient have heart disease base on the decision tree?
summary(m)

# Q9 9.	Boost the accuracy of decision trees by 
# boosted decision tree with 10 trials. 
# What is the average tree size for 10 iterations? 
# How many patients in the boosted model are misclassified? 
m_boosted <- C5.0(data.train[-14], data.train$statlogheart, trials = 10)
summary(m_boosted)
m_boosted

# Q10 10.	Create a cost sensitive decision tree on training data. 
# The sequence of the costs should be c (0, 1, 10, 0) in the R syntax. 
error_cost <- matrix(c(0,10,10,0), nrow = 2)
m_cost <- C5.0(data.train[-14], data.train$statlogheart, costs = error_cost)


# Q11 11.	Use the above cost sensitive decision tree 
# to create a factor vector of predictions for in-sample data. 
# How many cases of false negatives are there? 
# Is there any difference in CrossTable results 
# and contingency matrix in summary command?
m_cost_pred <- predict(m_cost, data.train)
summary(m_cost_pred)
library(gmodels)
CrossTable(data.train$statlogheart, m_cost_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('acutal statlogheart', 'predicted statlogheart'))
