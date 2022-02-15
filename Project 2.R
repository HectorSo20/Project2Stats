#SAT PERFORMANCE 2018-19 and 2019-20, for h.s. seniors in MA
install.packages("e1071") 
library(e1071)
library('pwr')

#Cleaning up the data, remove empty values; data == 2018-19.csv
data <- read.csv("//Users/hec20/Desktop/UMass - Amherst /First year/Courses/Fall 2021/PSY 640 - Stats/Project/Project 2/sat_performance 2018-19.csv")
# You can also do read.csv("sat_performance 2018-19.csv") if the csv file is in the same folder
data <- subset(data, select= -X.3) #remove empty column
#Column X.1 is School code, X.2 is the Reading/Writing Score 
#   and X.4 is Math Score
data<- subset(data, X.2 != "" & X.4 != "") #recreating the data w/o missing 
                                          #  reading/writing and math scores


#Adding a new column to the 2018-19 csv, that totals SAT score
rwScores <- data[,4] 
rwScores <- as.integer(rwScores[-1]) #removes the first item within the list "Reading/Writing" 
mathScores <- data[,5]
mathScores <- as.integer(mathScores[-1]) #removes the first item of the list "Math"
totalScores <- rwScores + mathScores
newColumn <- c("Total Scores",as.character(totalScores))
data$newColumn <- newColumn

#Cleaning up the data in the 2019-20 csv; data1 == 2019-20.csv
data1 <- read.csv("/Users/hec20/Desktop/UMass - Amherst /First year/Courses/Fall 2021/PSY 640 - Stats/Project/Project 2/sat_performance 2019-20.csv")
data1 <- subset(data1, select= -X.3)
data1<- subset(data1, X.2 != "" & X.4 != "") 

#Adding a column to the 2019-20 csv, total SAT scores 
rwScores1 <- data1[,4]
rwScores1 <- as.integer(rwScores1[-1])
mathScores1 <- data1[,5]
mathScores1 <- as.integer(mathScores1[-1])
totalScores1 <- rwScores1 + mathScores1
newColumn1 <- c("Total Scores",as.character(totalScores1))
data1$newColumn1 <- newColumn1

#Merge two dataframes
jointData <- merge(data, data1,by = c('X')) #Combined both CSV files by school codeum
jointData <- na.omit(jointData) #Remove rows that contain NA
colnames(jointData) = c("School Code", "2018-19 High Schools", "Tests Taken", 
                        "Reading/Writing Scores", "Math Scores", "Total Scores",
                        "2019-20 High Schools", "Tests Taken 1", 
                        "Reading/Writing Scores1", "Math Scores1", "Total Scores1")
jointData <- head(jointData, -1) #removes the last line which was old column
write.csv(jointData,
          "/Users/hec20/Desktop/UMass - Amherst /First year/Courses/Fall 2021/PSY 640 - Stats/Project/Project 2/jointData.csv")

#getting needed data groups for correlated t-tests
tT <- as.integer(jointData$`Tests Taken`) #transforming values to integer
tT1 <- as.integer(jointData$`Tests Taken 1`)
RW <- as.integer(jointData$`Reading/Writing Scores`)
RW1 <- as.integer(jointData$`Reading/Writing Scores1`)
math <- as.integer(jointData$`Math Scores`)
math1 <- as.integer(jointData$`Math Scores1`)
n <- length(math) #total number of rows

##These are the descriptive statistics of SAT scores
#### Summary of values #####
#Test taken summary 
summary(tT)
summary(tT1)

IQR(tT) #we see a large difference for the Interquartile Range
IQR(tT1) 

#RW Summary 
summary(RW) #2018-19
summary(RW1) #2019-20

IQR(RW) 
IQR(RW1) #4 point difference in IQR

#Math summary 
summary(math)
summary(math1) 

IQR(math)
IQR(math1) #3 Point difference in IQR


########Means for each column#########
meanT <- mean(tT)
meanT1 <- mean(tT1)
# In 2018-19, the mean number of SAT Tests Taken was 
meanT
# In 2019-20, the mean number of SAT Tests Taken was
meanT1

#2018-19 mean RW scores
meanRW <- mean(RW)
meanRW
#2019-20 mean RW scores 
meanRW1 <- mean(RW1)
meanRW1

#2018-19 mean Math scores
meanM <- mean(math)
meanM
#2019-20 mean Math scores
meanM1 <- mean(math1)
meanM1


####### GRAPHS #####
boxp_test <- boxplot(tT, tT1, col=c("skyblue1","red3"), 
                     names=c("2018-19","2019-20" ),
                     main="Average Number of SAT Tests Taken", 
                     xlab="MA Highschools",
                     ylab="Reading/Writing SAT Scores")
boxp_RW <- boxplot(RW, RW1, col=c("skyblue1","red3"), 
                   names=c("2018-19","2019-20" ),
                   main="Average Reading and Writing Scores", 
                   xlab="MA Highschools",
                   ylab="Reading/Writing SAT Scores")
boxp_math <- boxplot(math, math1, col=c("skyblue1","red3"),
                     names=c("2018-19","2019-20"),
                     main="Average Math Scores", 
                     xlab="MA Highschools", 
                     ylab="Math SAT Scores")

#These are transformed graphs to see if there are any differences 
boxp_test_log <- boxplot(log(tT), log(tT1), col=c("skyblue1","red3"), 
                     names=c("2018-19","2019-20" ),
                     main="log(Average Number of SAT Tests Taken)", 
                     xlab="MA Highschools",
                     ylab="log(Reading/Writing SAT Scores)")
  #we see a difference in log scores
boxp_RW_log <- boxplot(log(RW), log(RW1), col=c("skyblue1","red3"), 
                   names=c("2018-19","2019-20" ),
                   main="log(Average Reading and Writing Scores)", 
                   xlab="MA Highschools",
                   ylab="log(Reading/Writing SAT Scores)")
boxp_math_log <- boxplot(log(math), log(math1), col=c("skyblue1","red3"),
                     names=c("2018-19","2019-20"),
                     main="log(Average Math Scores)", 
                     xlab="MA Highschools", 
                     ylab="log(Math SAT Scores)")
#We see a difference in graphs between the number of SAT exams taken

######Levene's test to see if there is homogeneity of variance######
library('car')
#Homogeneity of variance for number of tests take
x_tests <- data.frame(d=c(tT, tT1), g=as.factor(
  c(replicate(n=336, 0), replicate(n=336, 1))))

leveneTest(x_tests$d, x_tests$g)
#We have significance, meaning there is more variability in one group than the other
  #Need to keep this in mind for t-test

#Testing homogeneity of variance for Reading/Writing Scores
x_rw <- data.frame(d=c(RW, RW1), g=as.factor(
  c(replicate(n=336, 0), replicate(n=336, 1))))

leveneTest(x_rw$d, x_rw$g)
#There is no difference between the variability of the two groups

#Testing homogeneity of variance for Math Scores
x_math <- data.frame(d=c(math, math1), g=as.factor(
  c(replicate(n=336, 0), replicate(n=336, 1))))

leveneTest(x_math$d, x_math$g)
#There is no difference between the variability of the two groups



####### Variance ###########
varT <- var(tT); varT
varT1 <- var(tT1); varT1
varRW <- var(RW); varRW
varRW1 <- var(RW1); varRW1
varM <- var(math); varM
varM1 <- var(math1); varM1


######Difference between the two points in time (including mean, sd)############
diffT <- tT - tT1 
diffRW <- RW - RW1 
diffM <- math - math1

mdT <- mean(diffT) #mean difference of each set of information
mdRW <- mean(diffRW)
mdM <- mean(diffM)



######Effect size#################
#Effect size for Tests Taken
s2_pooled_t <- ((n-1)/(n+n-2)*sd(tT)^2) + ((n-1)/(n+n-2)*sd(tT1)^2) #pooled varaince
s2_pooled_t
s_pooled_t <- sqrt(s2_pooled_t)
s_pooled_t

d_t <- (meanT - meanT1)/s_pooled_t
d_t
dz <- (meanT - meanT1)/sd(diffT)
dz

#Effect size for Reading/Writing Scores
s2_pooled_RW <- ((n-1)/(n+n-2)*sd(RW)^2) + ((n-1)/(n+n-2)*sd(RW1)^2) #assuming different sample sizes
s2_pooled_RW
s_pooled_RW <- sqrt(s2_pooled_RW)
s_pooled_RW

d_RW <- (meanRW - meanRW1)/s_pooled_RW
d_RW
dzRW <- (meanRW - meanRW1)/sd(diffRW)
dzRW


#Effect size for Math scores
s2_pooled_m <- ((n-1)/(n+n-2)*sd(math)^2) + ((n-1)/(n+n-2)*sd(math1)^2) #assuming different sample sizes
s2_pooled_m
s_pooled_m <- sqrt(s2_pooled_m)
s_pooled_m

d_m <- (meanM - meanM1)/s_pooled_m
d_m

dzM <- (meanM - meanM1)/sd(diffM)
dzM



######T-Test#########

t.test(tT, tT1, alternative = "two.sided", var.equal=FALSE, paired = TRUE)
"The true mean of the population is between 100.0061 118.4701" 
#The results of this t test tells us there were more SAT exams taken during 2018-19

t.test(RW, RW1, alternative = "two.sided", var.equal = FALSE, paired = TRUE) 
#negative c.i. tells us that x is less than y, better performance during 2019-20

t.test(math, math1, alternative = "two.sided", var.equal = FALSE, paired = TRUE)
#negative c.i. and results tell us there was better performance during 2019-20 year














