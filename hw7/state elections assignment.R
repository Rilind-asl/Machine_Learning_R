# 1.	Import the data into R and change the first variable, State, into a factor variable. 
data <- read.csv(file.choose(), stringsAsFactors = F)
data$State <- factor(data$State)

# 2.	Check summary of votes' percentage for the year 1856. What is the minimum percentage of vote? What is the median percentage of vote? How many missing values are in year 1856? 
summary(data$X1856)
# minimum percentage of votes is 0.19%
# median percentage of votes is 47.31%
# we have 30 NA's or missing values in 1856

# 3.	Find the mean votes' percentage for the year 1936 using function mean ( ). 
# Hint: you need to remove missing values, ie na.rm=TRUE
mean(data$X1936, na.rm=TRUE)
# 32.86646% mean votes' percentage for year 1936

# 4.	Create an imputing function to replace missing values in a given column with the mean value for that column after removing missing values. 
# Hint: You need to define a function, say impute 
# a.	that takes a column parameter (say x). 
# b.	uses ifelse on this column parameter. Condition is if a record is na then replace it with mean of this column after removing missing values; If record is not na then keep the record as it is. 
impute <- function(x){
  m <- mean(data[[x]], na.rm=TRUE)
    ifelse(is.na(data[[x]]), m, data[[x]])
}

# 5.	Check if impute function works for X1856.  
data$X1856 <- impute(2)
data[[2]]

# 6.	Apply impute function on all of the columns except the first column.
count <- 3 # start at 3rd column since we have the first column 
while(count <= 32){
  data[[count]] <- impute(count)
  count <- count+1
}


# 7.	Scale the variables in the new data frame.
data.norm <- data.frame(lapply(data[2:32], scale))

# 8.	Set seed (e.g. set.seed(5)) for fixed clustering results. Train a k-means clustering models on the scaled election data frame with k =5. 
# Note: Remember not to include the 1st State column while running kmeans. 
set.seed(5)
myclusters <- kmeans(data.norm, 5)


# 9.	How many States are in cluster 3 and 4? 
#   Hint: Add model$cluster to data frame. Make sure that column State is also there in the data frame. 

myclusters$size

# cluster 3: 17 states
# cluster 4: 4 states

# 10.	List out all states for the cluster 3
count <- 1
for(y in data$cluster){
  if(y == 3){
      print(data[count,1])
  }
  count = count + 1 
}
# this lists all states that are in cluster 3, I wasn't sure if there was an easier way to do it.


# 11.	Look at the center of clusters. What center values of x1876 do cluster 1, 2 and 3 have? 
myclusters$centers
# in 1876 the center values for c1: 0.53674150, c2: -0.70269844, c3: 0.06766413


# 12.	What are the average votes' percentages of year 1900 and 1892 for cluster 5? 
aggregate(data = data, data$X1892 ~ data$cluster, mean)
# for 1892 average votes' percentage was 17.46571
aggregate(data = data, data$X1900 ~ data$cluster, mean)
# for 1900 average votes' percentage was 23.83286

# 13.	Which clusters do Alabama, California and Utah belong to? 
#   Hint:you need to subset data for which you need to show cluster number, ie dt2$State==c("California", "Utah")
data[c(1,33)]
# alabama belongs to cluster 5
# california belongs to cluster 3
# utah belongs to cluster 3