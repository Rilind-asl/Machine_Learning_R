data <- read.csv(file.choose(), stringsAsFactors = F)
getwd()
setwd("C:/Users/albo1_000/Documents/R_WorkingDirectory")
getwd()
str(data)
summary(data$Price)
summary(data$Age)
summary(data$KM)
summary(data$Weight)
mean(data$HP)
median(data$HP)
range(data$Price)
diff(range(data$Price))
IQR(data$Price)
quantile(data$Price, c(0,0.2,0.4,0.6,0.8,1))
boxplot(data$Age, main="Boxplot of Toyota Car Ages", ylab="Ages (mons.)")
boxplot(data$KM, main="Boxplot of Toyota Car KM", ylab="KM")
hist(data$Price, main="Spread of Price", xlab="Prices", ylab="Count")
hist(data$Weight, main="Spread of Weight", xlab="weight", ylab="Count")
var(data$Price)
sd(data$Price)
table(data$MetColor)
table(data$Doors)
table(data$FuelType)
prop.table(table(data$FuelType))*100
plot(data$KM, data$Price, main="Scatter plot between KM and Price", xlab="KM")
install.packages("gmodels")
library("gmodels")
CrossTable(data$Automatic, data$Doors)
