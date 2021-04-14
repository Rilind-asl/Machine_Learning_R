# 1.	Load the music data into a sparse matrix. Since each user may mention one artist multiple times, we set "rm.duplicates=TRUE" when we load the data into a sparse matrix.
install.packages('arules')
library(arules)
data <- read.transactions(file.choose(), sep = ",", rm.duplicates = TRUE)


# 2.	Use summary () function to inspect the sparse matrix. How many users and columns do this matrix have? What does density mean? Who is the most frequent artist/item? How many rows/users have 30 items/artists? 
summary(data)
# 14593 rows refer to the users and 1004 columns 
# 1.975% density refers to the proportion of non-zero cells
# radiohead is the most frequent band of artists 
# element 30: size 340

# 3.	Inspect the first 5 users' artists. Does the records match the first five rows of the original csv file? Use itemFrequency to check the proportion of users liking certain artists. What are the proportions of users' favorite artists in column from 4 to 7? 
inspect(data[1:5])
# The records do match first five rows but they are listed in alphabetical order. 
itemFrequency(data[ ,4:7])
# 3.18% of users' favorite artists are 3 doors down
# 3.37% of users' favorite artists are 30 seconds to mars
# 0.85% of users' favorite artists are 311
# 0.81% of users' favorite artists are 36 crazyfists


# 4.	Create a histogram plotting the artists have more than 10 percent support. How many artists in the matrix have at least 10 percent support? Who are these artists? Plot the first 20 artists with highest support. Which artist has the 15th highest support?
itemFrequencyPlot(data, support = 0.1)
# 10 artists have at least 10% support
# the aritsts(bands) are coldplay, linkin park, metallica, muse, nirvana, pink floyd, radiohead, red hot chili peppers, the beatles, the killers.
itemFrequencyPlot(data, topN = 20)
# metallica is the 15th highest support

# 5.	Generate a visualization of the sparse matrix for the first 100 users' preference. Then generate a visualization of a random sample of 500 users' music favorite. 
image(data[1:100])
image(sample(data, 500))


# 6.	Use apriori() function to train the association rules on the music data. How many rules do we have when we use the default setting? In order to learn more rules, we adjust support level to 0.01, minlen =2 and confidence level to 0.25. How many rules do we have then?
myrules <- apriori(data = data)
myrules
# with default settings, we end up with 0 rules
myrules <- apriori(data = data, parameter = list(support = 0.01, confidence = 0.25, minlen = 2))
myrules
# 788 rules 

# 7.	Summarize the rules generated from adjusted parameters. How many rules have size 3 among all the rules? Check the first ten rules. If a user likes james blunt, which artist else should online radio recommend to this user? Sort the rule list by lift and inspect the first five rules with highest lift. Which rule has the fourth highest lift?
summary(myrules)
# 224 rules have size 3
inspect(myrules[1:10])
inspect(myrules[1:20])
# if the user likes james blunt they should like coldplay
inspect(sort(myrules, by = "lift")[1:5])
# 4th highest life rule is if users like beyoncc they will like rihanna
# [4] {beyoncc} => {rihanna} 0.01432194 0.4696629  0.03049407 10.64253 209 

# 8.	Find subsets of rules containing any cold play. How many rules have cold play? Sort the rules by support and inspect the first five cold play rules with highest support. What rule has the 2nd highest support?
coldplayrules <- subset(myrules, items %in% "coldplay")
coldplayrules
# 172 rules for coldplay
inspect(coldplayrules[1:5])
# second highest coldplay rule is if users like james blunt they should like coldplay
# [2] {james blunt} => {coldplay} 0.01199205 0.4629630  0.02590283 2.843442 175

# 9.	You can write these rules to a file and save the rules into a data frame. How many variables are in this saved data frame?
write(myrules, file = "myrules.csv", sep = ",", quote = TRUE, row.names = FALSE)
myrules_df <- as(myrules, "data.frame")
str(myrules_df)
# 788 observations of 6 variables
