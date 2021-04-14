# 1.	Import the monthly average Bitcoin price data into R. Check the summary of variable Monthly.Average.Price. What is the minimum average price of Bitcoin? What is the maximum average price of Bitcoin?
data <- read.csv(file.choose(), stringsAsFactors = FALSE)
summary(data$Monthly.Average.Price)
# minimum: 0.0619, Maximum: 857.1822

# 2.	Install the package "forecast" and create a time series object tprice. Select and only include the values from variable Monthly.Average.Price to construct this object. Set start month as c(2010, 7) and frequency = 12.
ts <- ts(data=data$Monthly.Average.Price, start=c(2010, 7), frequency=12)
start(ts)
end(ts)
frequency(ts)

# 3.	Check the object tprice and see if you have the correct prices and corresponding months. Plot the object tprice. At which year does the price of Bitcoin have a peak? What is the start and end month/year of tprice? What is the frequency of tprice?
window(ts, start=c(2010, 7), end=c(2015, 3))
plot(ts)
# Jan 2014 Bitcoin peaked
# Start is july 2010
# End is march 2015
# frequency is 12

# 4.	Create a subset of object tprice. Set the start month as July-2013 and end month as Feb-2015. Plot this subset. Hint: Use window method
tprice <- window(ts, start=c(2013, 7), end=c(2015, 2))
plot(tprice)

# 5.	Try smoothing and plotting and data using function ma( ). Try k=5, 10 and 15 respectively, do you see a smoother plotted curve with increasing k value?
library(forecast)
k <- 5
fit <- ma(ts, k)
plot(fit)
k <- 10
fit <- ma(ts, k)
plot(fit)
k <- 15
fit <- ma(ts, k)
plot(fit)
# as k increases the curve is smoother

# 6.	Next try seasonal decomposition using stl().What trend do you see in the data? 
lts <- log(ts)
sd <- stl(lts, s.window="period")
plot(sd)
# seasonal graph shows that it goes up sharply then it goes down sharply

# 7.	Visualize the seasonal decomposition by using seasonplot functions on the object tprice.  What are the price trends of Bitcoin in year 2013 and 2014? Do these two years share the same trend?
par(mfrow=c(2, 1))
seasonplot(ts, year.labels="TRUE")
# the value at the end of 2013 peaked the price of bitcoin, at the start of the year in 2014 it was at it's peak and then it started to drop

# 8.	Build a simple exponential smoothing forecasting model using time series object tprice with model = "ANN". What is the value of alpha parameter, ie smoothing parameter for the level? 
fit <- ets(ts, "ANN")
fit
accuracy(fit)
# smoothing parameters at 0.9999

# 9.	Use the forecast() function to predict the time series one step into the future. What is the average predicted Bitcoin price for April-2015? What is the 95% confidence interval for this prediction value? Plot this prediction with "Month" as x label and "Price" as y label. 
predicted <- forecast(fit, 1)
plot(predicted)
predicted
predicted$mean
predicted$upper
predicted$lower
# average in april-2015: 274.3764
# 95% confidence: 428.5784
plot(predicted, xlab="Month", ylab="Price")

# 10.	Check the accuracy of this simple model for time series forecasts. What do RMSE, MAE and MAPE stand for? What value of mean absolute percentage error does this model generate?
accuracy(fit)
# RMSE is root mean squared error
# MAE is mean absolute error
# MAPE is mean absolute percentage error
# the MAPE generated is 40.45797 or 4045.797%

# 11.	Log transform the data and save it as ltprice. Build an exponential smoothing model with Level, Slope, and Seasonal components with ltprice and model="ZZZ". Check the model. What are the values of smoothing parameters for the level, trend, and seasonal components? 
ltprice <- log(ts)
fit <- ets(ltprice, model="ZZZ")
fit
# Smoothing parameters: 0.9999
# no beta and gamma because it finds the best value to be ANN

# 12.	Use forecast() function to forecast the Bitcoin price for the next 5 months. Plot the prediction with "Month" as x label and "Price" as y label. Transform the mean, lower and upper prices of the prediction using exponential function to the actual predictions. What is the average predicted Bitcoin price for April-2015? What is the 95% confidence interval of the price?
predicted <- forecast(fit, 5)
plot(predicted, xlab="Month", ylab="Price")
predicted
predicted$mean <- exp(predicted$mean)
predicted$lower<- exp(predicted$lower)
predicted$upper <- exp(predicted$upper)
predicted$mean
predicted$upper
# april price: 274.3761
# 95% confidence interval: 668.8368

# 13.	Check the accuracy of this model for time series forecasts. What value is this model's mean absolute percentage error?
accuracy(predicted)
# MAPE: 36.1928 or 3619.28%

# 14.	Import library tseries. Decide the best d value for object ltprice using ndiffs( ). What is the best d value for our time series object? Then do the differencing of the time series object using diff( ). Plot the time series object after differencing. Does it look like there is trend in time series after differencing?   
library(forecast)
library(tseries)
plot(ltprice)
ndiffs(ltprice)

dltprice <- diff(ltprice)
plot(dltprice)
# There is no trend between ltprice and dltprice

# 15.	Evaluate the assumption of stationarity using Augmented Dickey-Fuller (ADF) test. Do we have a stationarity time series object based on the test results?
adf.test(dltprice)
# the object is stationary

# 16.	Fit an ARIMA model with p = 2, d=1 and q=1. What is the AIC value of this model? Then check the accuracy of the model. What is the value of MAPE?
fit <- arima(ltprice, c(2,1,1))
fit <- auto.arima(ltprice)
fit
# AIC: 50.83
accuracy(fit)
# MAPE: 28.29524 or 2829.524%

# 17.	Evaluate the model fitness by check the residuals using qqnorm and qqline functions. Does the residuals fall along the line? What can we learn if the residuals fall along the line? Use box.test() function to check whether autocorrelations are all zero. What can you interpret from the box.test() results?
qqnorm(fit$residuals)
qqline(fit$residuals)
# the residuals fall along the line, we can find if there is a trend
Box.test(fit$residuals, type="Ljung-Box")
# p-value is 0.8043 if it's more than 10-15% we reject the null hypothesis saying that there is no correlation

# 18.	Forecast three months Bitcoin prices with this ARIMA model. What is the predicted average Bitcoin price for April-2015? 
fit <- auto.arima(ts)
fit
predicted <- forecast(fit, 3)
predicted$mean
# average in april is 263.2202

# 19.	Use an automated ARIMA forecasting model for the object ltprice. What are the values for p, d and q? Compare this model and the one from Q16 based on AIC. Which of the two models is better base on AIC values?
fit <- auto.arima(ltprice)
fit
# p,d,q are 0,1,1 

# AIC for Q16 was 50.83 and AIC for Q19 they are also 50.83, therefore, I believe they are the same