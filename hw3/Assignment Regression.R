# Q1 1.	Import Boston Housing dataset from UCI machine learning 
# repository: assignment4.csv 
data <- read.csv(file.choose(), stringsAsFactors = F)

# Q2 2. Use str() to inspect the type and data frame of variables. 
# How many observations and how many variables in this dataset? 
# Learn the meaning of each variable.
str(data)

# Q3 3.	Reform the CHAS to nominal (factor) variable. 
# How many levels does CHAS have? How many instances belong to level 0? 
data$CHAS <- factor(data$CHAS)
str(data$CHAS)
table(data$CHAS)

# Q4 4. What is the mean value of age variable? 
# Since age is the proportion of homes built before 1940, 
# how do you interpret this mean value? What is the maximum 
# value of MEDV? Build a histogram for MEDV, 
# which value range has the highest frequency?
summary(data$AGE)
hist(data$MEDV)
summary(data$MEDV)

# Q5 5.	Check the correlation between all the variables except for CHAS.
# Which pairs have correlations larger than 0.6?
# What variables have correlations with MEDV larger than 0.6?
cor(data[c("CRIM", "ZN", "INDUS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")])


# Q6 6.	What are your assumptions of the relations between crime 
# and home value, between DIS and home value? 
# Does higher pupil-teacher ratio lead to lower home value? 
# Evaluate your assumptions with the results from correlation. 
# check correlation data above

# Q7 7.	Use pairs.panels() to visualize the relations between "RM", 
# "LSTAT" and "MEDV". What are the histograms depicting on the 
# diagonal? Does more room lead to higher home values? 
# Does higher percentage of lower 
# status of the population cause higher home values?
pairs(data[c("RM", "LSTAT", "MEDV")])
library(psych)
pairs.panels(data[c("RM", "LSTAT", "MEDV")])

# Q8 8.	Randomize the dataset and use the first 80% 
# of observations for training. Set seed as 1234
set.seed(1234)
data.r <- sample(data)
str(data.r)
nrows <- nrow(data.r)
train.size <- floor(nrows * 0.8)
data.train <- data.r[1:train.size,]
data.test <- data.r[-c(1:train.size),]

# Q9 9.	Build a linear regression model with MEDV as 
# dependent variable and the rest variables as independent variables. 
# Which variables have insignificant impact on MEDV?
# NUMERIC PREDICTION MODEL, THIS WILL PREDICT NUMBER NOT CLASSIFY
model <- lm(MEDV ~ B + TAX + RM + NOX + RAD + PTRATIO + ZN + DIS + CHAS + LSTAT + INDUS + AGE + CRIM, data = data.r)
summary(model)
# predicted MEDV = 36.89 + 0.0097*B - 0.0138*TAX... etc to get t value

# p-value or Pr(>|t|) probability of seeing such an extreme value
# due to randomness given that null hypothesis is true;
# rule if it is less than 5% it's statistical significant

# Q10 10.	How to interpret the estimate for variable RM? 
# What is the p-value? Is this variable statistically significant? Why? 
# the p-value of RM is < 2e-16
# RM is statistically significant at a 5% significance level 

# Q11 11.	What is the R-squared value and adjusted R-squared value?
# Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7338 
# tells how much of the variance you explain of the target variable
# adding more variables will increase r-squared or remain the same
# adjusted r-squared won't always increase and if the new variable doesn't help
# adjusted r-squared will go down.

# Q12 12.	Try Crim^2 as one of the dependent variables and 
# build a liner regression model include this variable 
# (all other variables included). 
# Does this Crim^2 have significant impact on MEDV? 
crim_model <- lm(CRIM^2 ~., data.r)
summary(crim_model)

# Q13 13.	Also create a new tax2 variable with tax2 = 1 if TAX >=350 
# and tax2 = 0 if TAX < 350. Build a linear regression model 
# include this tax2 (all other variables included). 
# What is the adjusted R-squared value?
data.r$tax2 <- ifelse(data.r$TAX >= 350, 1, 0)
tax2_model <- lm(tax2 ~., data.r)
summary(tax2_model)

# Q14 14.	Create a new simple regression model and 
# estimate MEDV using only CHAS and CRIM. 
new_crim_model <- lm(CRIM ~ MEDV + CHAS, data.r)
summary(new_crim_model)

# Q15 15.	How to interpret the estimate for variable CHAS1? 
# What is the p-value? Is this variable statistically significant? 
# Why? 
# refer to model above and it was not significant

# Q16 16.	Calculate MAE using the function as discussed in class 
# using training data set. Also, calculate the correlation 
# between predicted and the actual using training data set
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
library(rpart)
m <- rpart(MEDV ~ CRIM + CHAS, data = data.train)
p <- predict(m, data.train)
MAE(p, data.train$MEDV)
cor(p, data.train$MEDV)


        
# Q17 17.	Calculate MAE and correlation using testing data set. 

m <- rpart(MEDV ~ CRIM + CHAS, data = data.test)
p <- predict(m, data.test)
MAE(p, data.test$MEDV)
cor(p, data.test$MEDV)

# Q18 18.	Estimate MEDV via regression tree using CHAS and CRIM. 
library(rpart.plot)
m <- rpart(MEDV ~ CHAS + CRIM, data=data.r)
rpart.plot(m)
p <- predict(m, data.train)
MAE(p, data.train$MEDV)
cor(p, data.train$MEDV)
p <- predict(m, data.test)
MAE(p, data.test$MEDV)
cor(p, data.test$MEDV)

# Q19 19.	Estimate MEDV via model tree using CHAS and CRIM.  
library(RWeka)
m <- M5P(MEDV ~ CHAS + CRIM, data=data.r)
summary(m)

# using training set
m <- M5P(MEDV ~ CHAS + CRIM, data = data.train)
p <- predict(m, data.train)
MAE(p, data.train$MEDV)
cor(p, data.train$MEDV)

# using the testing set

m <- M5P(MEDV ~ CHAS + CRIM, data = data.test)
p <- predict(m, data.test)
MAE(p, data.test$MEDV)
cor(p, data.test$MEDV)

# comparing model tree vs regression tree vs simple regression model

simple_reg_model <- lm(MEDV ~ CRIM + CHAS, data.r)
predicted_simple <- predict(simple_reg_model, data.r)
MAE(predicted_simple, data.r$MEDV)
cor(predicted_simple, data.r$MEDV)

reg_tree_model <- rpart(MEDV ~ CRIM + CHAS, data.r)
reg_tree_predict <- predict(reg_tree_model, data.r)
MAE(reg_tree_predict, data.r$MEDV)
cor(reg_tree_predict, data.r$MEDV)

model_tree <- M5P(MEDV ~ CRIM + CHAS, data = data.r)
model_tree_pred <- predict(model_tree, data.r)
MAE(model_tree_pred, data.r$MEDV)
cor(model_tree_pred, data.r$MEDV)
