##### IS 4482 Homework 3 ----------------------------------------------------------

#### Regressions and BlackBox Methods ---------------------------------------------


# Part 1. Classification with BartRider data
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
train_index <- createDataPartition(BartRider$Rider, p=0.7, list=FALSE)
datTrain <- BartRider[train_index,]
datTest <- BartRider[-train_index,]

# B.	Show the overall structure and summary of train and test sets. Show the distributions of Rider in the entire set, the train set and the test set.
str(datTrain)
summary(datTrain)
str(datTest)
summary(datTest)
prop.table(table(BartRider$Rider))
prop.table(table(datTrain$Rider))
prop.table(table(datTest$Rider))

### 3. SVM for classification (5 points)

# A. Build a SVM model with C=5
library(kernlab)
library(rminer)
svm_model <- ksvm(Rider~.,data=datTrain, C=5)

# B. Generate this model's confusion matrices and classification evaluation metrics in training and testing sets.
prediction_on_train <- predict(svm_model, datTrain)
prediction_on_test <- predict(svm_model, datTest)

mmetric(datTrain$Rider,prediction_on_train, metric="CONF")
mmetric(datTest$Rider,prediction_on_test, metric="CONF")
mmetric(datTrain$Rider,prediction_on_train,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,prediction_on_test,metric=c("ACC","PRECISION","TPR","F1"))

### 4. Neural Network for classification (6 points)

# A. Build a MLP model with N=100, H='8, 4'
library(RWeka)
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
mlp_model <-  MLP(Rider~.,data=datTrain, control = Weka_control(N=100, H='8, 4'))
summary(mlp_model)

# B. Generate this model's confusion matrices and classification evaluation metrics in training and testing sets.
prediction_on_train2 <- predict(mlp_model, datTrain)
prediction_on_test2 <- predict(mlp_model, datTest)

mmetric(datTrain$Rider,prediction_on_train2, metric="CONF")
mmetric(datTest$Rider,prediction_on_test2, metric="CONF")
mmetric(datTrain$Rider,prediction_on_train2,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,prediction_on_test2,metric=c("ACC","PRECISION","TPR","F1"))

  # Which model is better for identifying riders, SVM or MLP?
  # MLP is better for identifying riders, because it has higher TPR2 value (recall value on rider=yes class).




# Part 2. Numeric Prediction with Game Sale data.
### -------------------------------------------------------------------------------
# Video games are a billion-dollar business and have been for many years. In 2016, 
# the video game market in the United States was valued at 17.68 billion U.S. dollars. 
# That same year U.S. consumers were said to spend roughly double the amount on gaming 
# content, hardware and accessories. What is important is that the first generation 
# of gamers is now grown up with significant spending power; therefore, despite high 
# penetration rates among kids, video games can no longer be considered solely a child’s 
# play. In fact, it was found that video gaming is gaining on popularity among the seniors 
# in the United States.

# This data provides video sales information together with game scores and ratings.
# Our task is to predict game sales by given the information of each game.

# VARIABLE DESCRIPTIONS:
# Name: Game name
# Platform: Platform of the games release (i.e. PC,PS4, etc.)
# Genre: Genre of the game
# Global_Sales: Total worldwide sales (in millions)
# Critic_Score: Aggregate score compiled by Metacritic staff
# Critic_Count: The number of critics used in coming up with the Critic_score 
# User_Score: Score by Metacritic’s subscribers
# User_Count: Number of users who gave the user_score
# Rating: The ESRB ratings
### ------------------------------------------------------------------------------

### 1. Import and clean data
# A. Import data. Load character variable as character strings first (stringsAsFactors = FALSE).
Sales <- read.csv(file = "sales.csv", stringsAsFactors = FALSE)

# B.	Show the overall structure and summary of the input data.
str(Sales)
summary(Sales)

# C. Remove the Name column
Sales$Name <- NULL

  # Why we should remove the name column? (1 points)
  # The Name column doesn't provide useful information for predicting Global_Sales. And we will have too many dummy variables if Name column is included.

# D. Transform Platform, Genre, and Rating to factor variables. Show the overall structure and summary of the input data again.
Sales$Platform <- factor(Sales$Platform)
Sales$Genre <- factor(Sales$Genre)
Sales$Rating <- factor(Sales$Rating)
str(Sales)
summary(Sales)

### 2. Data partitioning
# A.	Partition the data set for simple hold-out evaluation - 70% for training and the other 30% for testing.
set.seed(100)
train_index <- createDataPartition(Sales$Global_Sales, p=0.7, list=FALSE)
datTrain_sale <- Sales[train_index,]
datTest_sale <- Sales[-train_index,]

### 3. Multiple Linera Regression model for predicting Global_Sales value. (15 points)
# A. Build a Multiple Linera Regression model with lm function
lm_model <- lm(Global_Sales~., data = datTrain_sale)
summary(lm_model)

  # Why we have multiple Coefficients for Platform, Genre, and Rating variables?
  # The regression model creates several dummy variables for each categorical variable (Platform, Genre, and Rating), and each dummy variable has a corresponding coefficient.

  # How Critic_Score affects Global_Sales value? e.g., How Global_Sales changes if we increase Critic_Score by 1?
  # Critic_Score is positive correlated to Global_Sales. Increasing Critic_Score by 1, Global_Sales increases 0.01942 (in millions).

  # How Critic_Count affects Global_Sales value? e.g., How Global_Sales changes if we increase Critic_Count by 1?
  # Critic_Count is positive correlated to Global_Sales. Increasing Critic_Count by 1, Global_Sales increases 0.01285 (in millions).  

  # Are User_Score and User_Count important for predicting Global_Sales, and why? hint: using the p-value to determine the importance.
  # User_Score and User_Count have small P-values, they are statistically significant. Thus, User_Score and User_Count are important for predicting Global_Sales.

  # What is the value of Multiple R-squared? Interpret the meaning of this Multiple R-squared value.
  # Multiple R-squared value equals to 0.3338, which means 33.38% of the variation of dependent variable (Global_Sales) can be explained by independent variables.

# B. Generate this model's evaluation metrics on both training and testing data.
prediction_on_train3 <- predict(lm_model, datTrain_sale)
prediction_on_test3 <- predict(lm_model, datTest_sale)
mmetric(datTrain_sale$Global_Sales,prediction_on_train3,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest_sale$Global_Sales,prediction_on_test3,metric=c("MAE","RMSE","MAPE","RAE"))

  # How is Multiple Linera Regression performed compared to mean estimator?
  # Multiple Linera Regression is more effective than the mean estimator, because it makes 83% of the errors made by mean estimator.

  # Which evaluation metric tells you the percentage error? Interpret the meaning of percentage error value on testing data.
  # MAPE. The predictive errors made by Multiple Linera Regression is about 3 times of the original Global_Sales values.


### 4. Model Tree for predicting Global_Sales value. (7 points)
# A. Build a Model Tree with M5P function.
M5P_model <- M5P(Global_Sales~.,data = datTrain_sale)
M5P_model

# B. Generate this model's evaluation metrics on both training and testing data.
prediction_on_train_4 <- predict(M5P_model, datTrain_sale)
prediction_on_test_4 <- predict(M5P_model, datTest_sale)
mmetric(datTrain_sale$Global_Sales,prediction_on_train_4,metric = c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest_sale$Global_Sales,prediction_on_test_4,metric = c("MAE","RMSE","MAPE","RAE"))

  # Compared to Multiple Linera Regression, does Model Tree have better performance, and why?
  # Model Tree has better performance. Because it achieves lower values on all evaluation metrics.


### 5. SVM for predicting Global_Sales value. (7 points)
# A. Build a SVM model with your choice of C value.
svm_model2 <- ksvm(Global_Sales~.,data=datTrain_sale, C=1)

  # How you choose this C value to build the SVM model?
  # By comparing C =1, C=5, C=10. C=1 provides the best performance on testing data.

# B. Generate this model's evaluation metrics on both training and testing data.
prediction_on_train5 <- predict(svm_model2, datTrain_sale)
prediction_on_test5 <- predict(svm_model2, datTest_sale)

mmetric(datTrain_sale$Global_Sales,prediction_on_train5,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest_sale$Global_Sales,prediction_on_test5,metric=c("MAE","RMSE","MAPE","RAE"))


### 6. Neural Network for predicting Global_Sales value. (9 points)
# A. Build a MLP model with with N=100, H='8, 8'
mlp_model2 <-  MLP(Global_Sales~.,data=datTrain_sale, control = Weka_control(N=100, H='8, 8'))
summary(mlp_model2)

# B. Generate this model's evaluation metrics on both training and testing data.
prediction_on_train6 <- predict(mlp_model2, datTrain_sale)
prediction_on_test6 <- predict(mlp_model2, datTest_sale)

mmetric(datTrain_sale$Global_Sales,prediction_on_train6,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest_sale$Global_Sales,prediction_on_test6,metric=c("MAE","RMSE","MAPE","RAE"))

# Assume that you will lose each dollar your model’s prediction misses due to an over-estimation or under-estimation. Which evaluation metric you should use?
# Based on your choice of evaluation metric, which model has better performance, SVM or MLP?
# We should use MAE to evaluate predictive models. SVM has better performance, because it has lower MAE value.


# Assume that the penalty for an erroneous prediction increases with the difference between the actual and predicted values. Which evaluation metric you should use?
# Based on your choice of evaluation metric, which model has better performance, SVM or MLP?
# We should use RMSE to to evaluate predictive models. SVM has better performance, because it has lower RMSE value.
