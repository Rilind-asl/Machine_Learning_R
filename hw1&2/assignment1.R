names <- c("Henry Grant", "Amy Becker", "Bessie Kim")
gender <- c("male", "female", "female")
gpa <- c(3.5, 3.7, 3.2)
grade <- c("senior", "sophomore", "junior")
names
gender
gpa
grade
gender <- factor(gender)
gender
grade <- factor(grade, levels=c("freshman", "sophomore", "junior", "senior"))
grade
gender[2]
grade[1:2]
data <- data.frame(names, gender, gpa, grade, stringAsFactor = F)
data
data$gpa
data
data[c(gpa)]
data[c("names","grade")]
data[c(1), c(2)]
data[c(2,3), c(3,4)]
data[3,]
data[2]
data
m <- matrix(c('a','b','c','d','e','f','g','h'), nrow=4)
m
m <- matrix(c('a','b','c','d','e','f','g','h'), ncol=2)
m
N <- matrix(c('a','b','c','d','e','f','g','h','i'), ncol=3)
N <- matrix(c('a','b','c','d','e','f','g','h','i'), nrow=3)
N
m[c(2),c(1)]
m
N[c(3),c(2)]
N
m[2,]
m
N[,3]
N
