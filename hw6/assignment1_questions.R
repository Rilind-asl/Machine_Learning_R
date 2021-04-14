# 1.	Import the data into R and change variables Brick and Neighborhood into factor variables. Eliminate variable Home ID from the original data frame.
data <- read.csv(file.choose(), stringsAsFactors = FALSE)
data$HomeID <- NULL
data$Brick <- factor(data$Brick)
data$Neighborhood <- factor(data$Neighborhood)

# 2.	Use aggregate function to inspect the mean of Price grouped by variable Neighborhood. What is the average price of houses in North neighborhood? What is the average price of houses in West neighborhood? 
# Or      aggregate(dt[1],dt[7],mean)

aggregate(data[1], data[7], mean)
# North average price: $110,154.50
# West average price: $159,294.90

# 3.	Overwrite variables Brick and Neighborhood into dummy variables. If Brick = "Yes", we recode the variable value into 1, or else we recode it into 0. For Neighborhood variable, if neighborhood is west, we recode the variable value into 1, or else we recode it into 0. 
data$Brick <- ifelse(data$Brick == 'Yes', 1, 0)
data$Neighborhood <- ifelse(data$Neighborhood == 'West', 1, 0)

# 4.	Generate a normalization function and apply this function to all variables in the House data frame. What are maximal and minimal values of SqFt? Confirm the range is now between zero and one. 
norm <- function(x){
  ifelse(is.numeric(x),
         return ((x - min(x))/(max(x) - min(x)) ),
         return (x)
  )
}

data <- data.frame(lapply(data, norm))
summary(data)
# Sqft max: 1
# Sqft min: 0

# 5.	Randomize the normalized data frame and create training and testing samples on randomized data frame. Select the first 70% of houses as training sample (you can round the number using floor() or other functions) and the rest 30% of houses as testing sample.
nrows <- nrow(data)
train.idx <- sample(1:nrows, floor(0.7*nrows))
data.train <- data[train.idx,]
data.test <- data[-train.idx,]


# 6.	Train a neuralnet model on training sample. Use 5 hidden neurons with Price as dependent variable and SqFt, Bedrooms, Bathrooms, Offers, recoded Brick and recoded Neighborhood as independent variables. 
# NOTE: in formula Price ~.  may not work. You may have to write down all columns on the right side, ie Price ~ SqFT + Bedrooms +.
library(neuralnet)
m <- neuralnet(Price ~ SqFt + Bedrooms + Bathrooms + Offers + Brick + Neighborhood, data=data.train, hidden = 1)


# 7.	Visualize the network topology by plotting the model. How many hidden nodes are included in the model? How many steps were used to build the model? What is the error value? 
plot(m)
# hidden nodes: 1
# steps: 399
# Error: 0.296887

# 8.	Obtain the model prediction by applying the model on testing sample. Get the predictions of house price and examine the correlation between prediction and actual house price from testing sample. What is the correlation between these two variables? 
p <- compute(m, data.test)
house_price_prediction <- p$net.result
cor(house_price_prediction, data.test$Price)
# correlation is 0.9113492 which indicates a strong linear relationship

# 9.	Make a residual plot (i.e. residuals on y-axis and fitted values on x-axis). Do you see any trend? 
# Hint: residuals<- dt.test$Price - predicted$net.result
residuals <- data.test$Price - p$net.result
plot(residuals)
# no trend 

# 10.	Now run the model using SVM on training dataset and use kernel='vanilladot'. Use this model to predict Price using testing dataset. Examine the correlation between prediction and actual house price from testing sample. What is the correlation between these two variables? 
library(kernlab)
m <- ksvm(Price ~ SqFt + Bedrooms + Bathrooms + Offers + Brick + Neighborhood, data=data.train, kernel = 'vanilladot')
predicted <- predict(m, data.test)
cor(predicted, data.test$Price)
# correlation is 0.9056848 which is highly linear