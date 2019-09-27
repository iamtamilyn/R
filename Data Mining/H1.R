##### IS 4482 Homework 1---------------------------------------------------------

#### BartRider Data Exploration and Decision Tree Classification-------------------

### -------------------------------------------------------------------------------
# You have been given a data file by the San Francisco Bay Area Rapid Transit (BART), 
# which identifies a set of demographics for residents in a local area. We will use 
# this file to determine if a resident is a rider.

# VARIABLE DESCRIPTIONS:
# Age:  1. 14 thru 17; 
#       2. 18 thru 24 
#       3. 25 thru 34
#       4. 35 thru 44
#       5. 45 thru 54
#       6. 55 thru 64
#       7. 65 and Over
# DistToWork: Distance to work in miles
# DualInc: Is dual income household or not
# Education:  1. Grade 8 or less
#             2. Grades 9 to 11
#             3. Graduated high school
#             4. 1 to 3 years of college
#             5. College graduate
#             6. Grad Study
# Gender:	M or F
# Income: 1. Less than $10,000
#         2. $10,000 to $14,999
#         3. $15,000 to $19,999
#         4. $20,000 to $24,999
#         5. $25,000 to $29,999
#         6. $30,000 to $39,999
#         7. $40,000 to $49,999
#         8. $50,000 to $74,999
#         9. $75,000 or more
# Language:	Language spoken at home
# NbrInHouseHold:	Number in household
# NbrInHouseholdUnder18:	Number in household under 18 years old
# OwnRent:	Own, rent, or live with parents
# YrsInArea:	Years lived in bay area
# Rider:	No, Non-rider; Yes, Rider
### ------------------------------------------------------------------------------



### 1. Import and clean data (10 points)
# A. Import data. Load character variable as character strings first (stringsAsFactors = FALSE).
BartRider <- read.csv(file = "BartRider.csv", stringsAsFactors = FALSE)

# B.	Show the overall structure and summary of the input data.
str(BartRider)
summary(BartRider)

# C.	Transform DualInc, Gender, Language, OwnRent, and Rider to factor variables. Show the overall structure and summary of the input data again.
BartRider$DualInc <- factor(BartRider$DualInc)
BartRider$Gender <- factor(BartRider$Gender)
BartRider$Language <- factor(BartRider$Language)
BartRider$OwnRent <- factor(BartRider$OwnRent)
BartRider$Rider <- factor(BartRider$Rider)

str(BartRider)
summary(BartRider)

# D.	Show the number of rows as well as the number of columns of input data.
nrow(BartRider)
ncol(BartRider)

# E.	Show the first 10 instances of input data. Also, show the last 10 instances of input data.
BartRider[1:10,]
tail(BartRider,10)

### 2. Code for exploring a single variable (10 points)
# A.	For each of the following variables - Age, and Education draw variable's boxplot. Include title in each plot.
boxplot(BartRider$Age, main="Boxplot of Age", ylab="Age")
boxplot(BartRider$Education, main="Boxplot of Education", ylab="Education")

# B.	For each of the following variables - Income, and YrsInArea draw variable's histogram. Include title in each plot.
hist(BartRider$Income, main = "Histogram of Income", xlab = "Income")
hist(BartRider$YrsInArea, main = "Histogram of Years in Bay Area", xlab = "Years in Bay Area")

# C.	For each of the following variables - DistToWork, NbrInHouseHold and NbrInHouseholdUnder18, show variable quantiles. 
quantile(BartRider$DistToWork)
quantile(BartRider$NbrInHouseHold)
quantile(BartRider$NbrInHouseholdUnder18)

# D.  Choose two factor variables. For each factor variable, show the number of instances (count) in each category/level.
summary(BartRider$Gender)
summary(BartRider$DualInc)

# E.  Choose two factor variables to plot. Include title in each plot.
plot(BartRider$OwnRent, main = "Plot of Own/Rent in Bay Area", xlab = "Own, rent, or live with parents")
plot(BartRider$Language, main = "Plot of Language Spoken at Home in Bay Area", xlab = "Lanaguage Spoken at Home")

### 3. Code to understand relationships amongst multiple variables (10 points)
# A.  Display correlations for DistToWork and NbrInHouseHold.
cor(BartRider[,c("DistToWork","NbrInHouseHold")])

# B.  Display correlations for all numeric variables.
cor(BartRider[,sapply(BartRider, is.numeric)])

# C.  Show a boxplot of Income by Rider.
boxplot(Income~Rider, data = BartRider)

# D.  Use the aggregate function with summary to aggregate DistToWork by Rider.
aggregate(DistToWork~Rider, summary, data = BartRider)

# E.  Show a boxplot of Income by Rider for only Female.
boxplot(BartRider[which(BartRider$Gender=="F"),6], main="Boxplot of Income by Female", ylab="Income")

### 4. Data partitioning and inspection code (10 points)
# A.	Partition the data set for simple hold-out evaluation - 50% for training and the other 50% for testing.
library(caret)
set.seed(1)
train_index <- createDataPartition(BartRider$Rider, p=0.5, list=FALSE)
datTrain = BartRider[train_index,]
datTest = BartRider[-train_index,]

# B.	Show the overall structure and summary of train and test sets. Show the distributions of Rider in the entire set, the train set and the test set.
nrow(datTrain)
nrow(datTest)
prop.table(table(BartRider$Rider))
prop.table(table(datTrain$Rider))
prop.table(table(datTest$Rider))

### 5. Simple decision tree training and testing. (10 points)
# A.	Train a C5.0 model using the default setting. 
# Generate this model's confusion matrices and classification evaluation metrics in testing and training sets.
library(C50)
library(rminer)
modelA <- C5.0(Rider~.,data=datTrain)
modelA
plot(modelA)
plot(modelA, type = "simple")
summary(modelA)

prediction_on_trainA <- predict(modelA, datTrain)
prediction_on_testA <- predict(modelA, datTest)

mmetric(datTrain$Rider,prediction_on_trainA, metric="CONF")
mmetric(datTrain$Rider,prediction_on_trainA,c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,prediction_on_testA, metric="CONF")
mmetric(datTest$Rider,prediction_on_testA,metric=c("ACC","PRECISION","TPR","F1"))

# B.	Explore reducing the tree complexity by the lowering CF levels. 
# In the code, select a CF level of your choice to train and test another C5.0 model.
# Generate this model's confusion matrices and classification evaluation metrics in testing and training sets.
modelB <- C5.0(Rider~.,data=datTrain, control = C5.0Control(CF = .7))
modelB
plot(modelB, type = "simple")
summary(modelB)

prediction_on_trainB <- predict(modelB, datTrain)
prediction_on_testB <- predict(modelB, datTest)

mmetric(datTrain$Rider,prediction_on_trainB, metric="CONF")
mmetric(datTrain$Rider,prediction_on_trainB,c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,prediction_on_testB, metric="CONF")
mmetric(datTest$Rider,prediction_on_testB,metric=c("ACC","PRECISION","TPR","F1"))


# Which model has better performance (model in task A or B), and why?
print("The performances are really close, but ModelB has a slightly higher accuracy score. Additionally, the F-meassure B (F12) is higher in ModelB, meaning it is more accurate at identifying if a resident is a Rider. ")


##### end-------------------------------------------------------------------------