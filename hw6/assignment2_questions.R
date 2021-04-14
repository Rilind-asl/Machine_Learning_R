# The objective of this data is utilizing the attributes 1-6 to predict the overall car evaluation.

# 1. Import the data into R and change all the variables into factor variables. You can directly change the variables into factor variables by setting "stringsAsFactors = TRUE" when you import the data. Inspect the data structure of the data. How many vehicles are included in this data set?
data <- read.csv(file.choose(), stringsAsFactors = TRUE)
# 1728 vehicles in the data set

# 2. How many vehicles have better evaluation than acceptable (not include acceptable)? For cars with big luggage, what proportion of "very good" evaluations do these cars have?  For all the vehicles having "very good" evaluations, what proportion of median safety do these cars have? How many vehicles were bought in very high price and have good evaluation? How many vehicles have high maintenance price and have very good overall evaluation?
summary(data)
# 69 good and 65 very good, which is a total of 134 vehicles have an evaluation better than acceptable
library(gmodels)
table(data$lug_boot, data$class)
# 40 cars with big luggage have a very good evaluation
table(data$class, data$safety)
# all vehicles with very good evaluation have a high safety rating
table(data$buying, data$class)
# 0 vehicles bought in the very high price range are good safety evaluations
table(data$class, data$maint)
# 13 vehicles have high maintenance and very good overall evaluation

# 3. Randomize the data set and generate training and testing samples on the randomized dataset. Select the first 70% of the vehicles as training sample (use floor () function or other functions to round the number) and the rest 30% of the vehicles as testing sample. 
nrows <- nrow(data)
train.idx <- sample(1:nrows, floor(0.7*nrows))
data.train <- data[train.idx,]
data.test <- data[-train.idx,]


# 4. Build a Support Vector Machine model on training sample with class variable as outcome variable and the rest attributes as predictors. Build this model using kernel = "vanilladot" and other default parameters. Look at the basic information about the model. How many support vectors does this model generate? What is the training error of this model?
library(kernlab)
m <- ksvm(class ~., data=data.train, kernel = 'vanilladot')
m
# generated vecotrs: 400
# training error is 0.06038

# 5. Evaluate the model. Use the model to predict the class labels of testing sample. Then use table or cross table to evaluate the performance. How many vehicles in total have been misclassified? How many vehicles have been classified as unacceptable are actually good? 
p <- predict(m, data.test)
table(p, data.test$class)
# misclassified: 37
# misclassified as unacc but are good: 0

# 6. Evaluate the model in another way. Look only at agreement vs. non-agreement and construct a vector of TRUE/FALSE indicating correct/incorrect predictions. How many "TRUE" agreements do the model generate for testing sample? What percentage does the "TRUE" agreements have among all agreements?
agreement <- p == data.test$class
table(agreement)
# 482 TRUE aggrements in the testing sample
prop.table(table(agreement))
# 92.87% TRUE

# 7. Build a new Support Vector Machine model on training sample with class variables as outcome variable and the rest attributes as predictors. Build this model using kernel = "rbfdot" and other default parameters. How many support vectors does this model generate? What is the training error of this model?
library(kernlab)
m <- ksvm(class ~., data=data.train, kernel = 'rbfdot')
m
# Support vecotrs: 577
# Error: 0.033912


# 8. Evaluate the new model. Use the model to predict the class labels of testing sample. Then use table or cross table to evaluate the performance. How many vehicles in total have been misclassified? How many vehicles have been classified as good are actually unacceptable? 
p <- predict(m, data.test)
table(p, data.test$class)
# misclassified: 32
# 0 vehicles are classified as good but are unacceptable
