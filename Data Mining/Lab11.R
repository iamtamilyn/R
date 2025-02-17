### -------------------------------------------------------------------------------
# You have been given a data file by the San Francisco Bay Area Rapid Transit (BART), 
# which identifies a set of demographics for residents in a local area. We will use 
# this file to determine residents segmentations so that we can use it to develop marketing
# plans accordingly.

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


# 1. Import the datadet
BartRider <- read.csv(file = "BartRider.csv", stringsAsFactors = FALSE)

# 2.	Show the overall structure and summary of the input data.
str(BartRider)
summary(BartRider)

# 3.	Transform DualInc, Gender, Language, OwnRent, and Rider to factor variables. Show the overall structure and summary of the input data again.
BartRider$DualInc <- factor(BartRider$DualInc)
BartRider$Gender <- factor(BartRider$Gender)
BartRider$Language <- factor(BartRider$Language)
BartRider$OwnRent <- factor(BartRider$OwnRent)
BartRider$Rider <- factor(BartRider$Rider)
str(BartRider)
summary(BartRider)

# 5. Partition the dataset: 70% for training, 30% for testing   
library(caret)
set.seed(1)
train_index <- createDataPartition(BartRider$Rider, p=0.7, list=FALSE)
datTrain <- BartRider[train_index,]
datTest <- BartRider[-train_index,]

# 6. Check the rows and porportion of target variable for both training and testing datasets
nrow(datTrain)
nrow(datTest)
prop.table(table(datTrain$Rider))
prop.table(table(datTest$Rider))

# 7.Build decision tree model with rpart model
library(rpart)
library(rminer)

rpart_model <- rpart(Rider~.,data = datTrain)
rpart_model

prediction_on_train_rpart <- predict(rpart_model, datTrain)
prediction_on_test_rpart <- predict(rpart_model, datTest)

mmetric(datTrain$Rider,prediction_on_train_rpart, metric="CONF")
mmetric(datTest$Rider,prediction_on_test_rpart, metric="CONF")
mmetric(datTrain$Rider,prediction_on_train_rpart,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,prediction_on_test_rpart,metric=c("ACC","PRECISION","TPR","F1"))

# 8. Bagging based on decision tree model
library(adabag)
set.seed(1)
bagging_model <- bagging(Rider~., data = datTrain, nbagg = 25)

prediction_on_train_bagging <- predict(bagging_model, datTrain)
prediction_on_test_bagging <- predict(bagging_model, datTest)
mmetric(datTrain$Rider,factor(prediction_on_train_bagging$class),metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,factor(prediction_on_test_bagging$class),metric=c("ACC","PRECISION","TPR","F1"))

# 9. AdaBoost based on decision tree model
adaboost_model <- boosting(Rider~., data = datTrain, mfinal=100)

prediction_on_train_adaboost <- predict(adaboost_model, datTrain)
prediction_on_test_adaboost <- predict(adaboost_model, datTest)
mmetric(datTrain$Rider,factor(prediction_on_train_adaboost$class),metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,factor(prediction_on_test_adaboost$class),metric=c("ACC","PRECISION","TPR","F1"))


# 10. Random forests
library(randomForest)
set.seed(1)

rf_model <- randomForest(Rider~., data = datTrain, ntree = 500, maxnodes = 200, mtry = 3)
rf_model
rf_model$importance

prediction_on_train_rf <- predict(rf_model, datTrain)
prediction_on_test_rf <- predict(rf_model, datTest)

mmetric(datTrain$Rider,prediction_on_train_rf, metric="CONF")
mmetric(datTest$Rider,prediction_on_test_rf, metric="CONF")
mmetric(datTrain$Rider,prediction_on_train_rf,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,prediction_on_test_rf,metric=c("ACC","PRECISION","TPR","F1"))

