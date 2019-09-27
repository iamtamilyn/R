##### IS 4482 Quiz 1---------------------------------------------------------------

#### BartRider Data----------------------------------------------------------------

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

### 1. Import and clean data
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

### 2. Data partitioning and inspection code
# A.	Partition the data set for simple hold-out evaluation - 70% for training and the other 30% for testing.
library(caret)
set.seed(100)
inTrain <- createDataPartition(BartRider$Rider, p=0.7, list=FALSE)
datTrain <- BartRider[inTrain,]
datTest <- BartRider[-inTrain,]

### 3. Simple decision tree training and testing.
# A.	Train a rpart decision tree model with cp = 0.0001, maxdepth = 2. 
library(rpart)
library(rpart.plot)
library(rminer)
rpart_model <- rpart(Rider~.,data=datTrain,control = rpart.control(cp = 0.0001, maxdepth = 2))
rpart.plot(rpart_model)
rpart_model

# B.  Make predictions on both training and tessting sets
prediction_on_train <- predict(rpart_model, datTrain)
prediction_on_test <- predict(rpart_model, datTest)

# C.  Generate this model's confusion matrices and classification evaluation metrics in testing and training sets.
mmetric(datTrain$Rider,prediction_on_train, metric="CONF")
mmetric(datTest$Rider,prediction_on_test, metric="CONF")
mmetric(datTrain$Rider,prediction_on_train,metric= c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,prediction_on_test,metric=c("ACC","PRECISION","TPR","F1"))

### 4. Naive Bayes training and testing.
# A.	Train a NB model with laplace = 1
library(e1071)
library(rminer)
modelNB <- naiveBayes(Rider~.,data=datTrain, laplace = 1)
modelNB

# B.  Make predictions on both training and tessting sets
prediction_on_trainNB <- predict(modelNB, datTrain)
prediction_on_testNB <- predict(modelNB, datTest)

# C.  Generate this model's confusion matrices and classification evaluation metrics in testing and training sets.
mmetric(datTrain$Rider,prediction_on_trainNB, metric="CONF")
mmetric(datTest$Rider,prediction_on_testNB, metric="CONF")
mmetric(datTrain$Rider,prediction_on_trainNB,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,prediction_on_testNB,metric=c("ACC","PRECISION","TPR","F1"))

# Which model has better performance (decision tree model or Naive Bayes model), and why?
# Please indicate which evaluation results (training or testing evaluation results) are used for comparison, and name the evaluation metrics selected to compare model performances.  
print("Using the testing results, the Decision tree has many higher evaluation metrics including the accurace and F-meausres representing the overal performance of the model and each class. ")

##### end-------------------------------------------------------------------------
