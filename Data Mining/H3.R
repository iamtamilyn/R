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
svm_model <- ksvm(Rider~.,data=datTrain)
svm_model

# B. Generate this model's confusion matrices and classification evaluation metrics in training and testing sets.
prediction_on_trainSVM <- predict(svm_model, datTrain)
prediction_on_testSVM <- predict(svm_model, datTest)

mmetric(datTrain$Rider,prediction_on_trainSVM, metric="CONF")
mmetric(datTest$Rider,prediction_on_testSVM, metric="CONF")
mmetric(datTrain$Rider,prediction_on_trainSVM,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,prediction_on_testSVM,metric=c("ACC","PRECISION","TPR","F1"))

### 4. Neural Network for classification (6 points)
# A. Build a MLP model with N=100, H='8, 4'
library(RWeka)
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
mlp_model <-  MLP(Rider~.,data=datTrain, control = Weka_control(N=100, H='8, 4'))
summary(mlp_model)

# B. Generate this model's confusion matrices and classification evaluation metrics in training and testing sets.
prediction_on_trainMLP <- predict(mlp_model, datTrain)
prediction_on_testMLP <- predict(mlp_model, datTest)

mmetric(datTrain$Rider,prediction_on_trainMLP, metric="CONF")
mmetric(datTest$Rider,prediction_on_testMLP, metric="CONF")
mmetric(datTrain$Rider,prediction_on_trainMLP,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Rider,prediction_on_testMLP,metric=c("ACC","PRECISION","TPR","F1"))


# Which model is better for identifying riders, SVM or MLP?
# The MLP model is better for identifying riders with a higher TPR2 at 92.634


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
# User_Score: Score by Metacritics subscribers
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
# The name does not have an impact on numeric prediction, we're only looking at the characterstics of the game. 

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
library(rminer)
lm_model_sale <- lm(Global_Sales~.,data=datTrain_sale)
summary(lm_model_sale)

  # Why we have multiple Coefficients for Platform, Genre, and Rating variables?
# There are several possible values for each field.
  # How Critic_Score affects Global_Sales value? e.g., How Global_Sales changes if we increase Critic_Score by 1?
# Critic_Score       1.942e-02 
  # How Critic_Count affects Global_Sales value? e.g., How Global_Sales changes if we increase Critic_Count by 1?
# Critic_Count       1.285e-02 
  # Are User_Score and User_Count important for predicting Global_Sales, and why? hint: using the p-value to determine the importance.
# Yes, they are small values and are likly less than the signifiance level which would make them statistically significant. 
  # What is the value of Multiple R-squared? Interpret the meaning of this Multiple R-squared value.
# Multiple R-squared:  0.3338
# The model explains about 33.38% of the variation in the dependent variable.

# B. Generate this model's evaluation metrics on both training and testing data.
prediction_on_train_sale <- predict(lm_model_sale, datTrain_sale)
prediction_on_test_sale <- predict(lm_model_sale, datTest_sale)

mmetric(datTrain_sale$Global_Sales,prediction_on_train_sale,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest_sale$Global_Sales,prediction_on_test_sale,metric=c("MAE","RMSE","MAPE","RAE"))

  # How is Multiple Linera Regression performed compared to mean estimator?
# Using the RAE (relative absolute error) which indicates model effectiveness compare to a simple mean estimator.
  # Which evaluation metric tells you the percentage error? Interpret the meaning of percentage error value on testing data.
# Mean absolute percentage error (MAPE)


### 4. Model Tree for predicting Global_Sales value. (7 points)
# A. Build a Model Tree with M5P function.
library(RWeka)
M5p_model_sale2 <- M5P(Global_Sales~.,data = datTrain_sale)
M5p_model_sale2

# B. Generate this model's evaluation metrics on both training and testing data.
prediction_on_train_sales2 <- predict(M5p_model_sale2, datTrain_sale)
prediction_on_test_sales2 <- predict(M5p_model_sale2, datTest_sale)

mmetric(datTrain_sale$Global_Sales,prediction_on_train_sales2,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest_sale$Global_Sales,prediction_on_test_sales2,metric=c("MAE","RMSE","MAPE","RAE"))

  # Compared to Multiple Linera Regression, does Model Tree have better performance, and why?
# The Model Tree is better, the RAE is improved, with a lower score.


### 5. SVM for predicting Global_Sales value. (7 points)
# A. Build a SVM model with your choice of C value.
library(kernlab)
svm_model_sale3 <- ksvm(Global_Sales~.,data=datTrain_sale, C=2)
svm_model_sale3

  # How you choose this C value to build the SVM model?
# Not going too high. When i tried a higher value like C=5, the differences between the training and testing data was increasing.
# I stuck with C=2 because it seemed to increase the results from C=1 without having much difference between training/testing.

# B. Generate this model's evaluation metrics on both training and testing data.
prediction_on_train_sales3 <- predict(svm_model_sale3, datTrain_sale)
prediction_on_test_sales3 <- predict(svm_model_sale3, datTest_sale)

mmetric(datTrain_sale$Global_Sales,prediction_on_train_sales3,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest_sale$Global_Sales,prediction_on_test_sales3,metric=c("MAE","RMSE","MAPE","RAE"))

### 6. Neural Network for predicting Global_Sales value. (9 points)
# A. Build a MLP model with with N=100, H='8, 8'
library(RWeka)
library(rminer)
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
mlp_model_sale4 <-  MLP(Global_Sales~.,data=datTrain_sale, control = Weka_control(N=100, H='8, 8'))
summary(mlp_model_sale4)

# B. Generate this model's evaluation metrics on both training and testing data.
prediction_on_train_sales4 <- predict(mlp_model_sale4, datTrain_sale)
prediction_on_test_sales4 <- predict(mlp_model_sale4, datTest_sale)

mmetric(datTrain_sale$Global_Sales,prediction_on_train_sales4,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest_sale$Global_Sales,prediction_on_test_sales4,metric=c("MAE","RMSE","MAPE","RAE"))

# Assume that you will lose each dollar your models prediction misses due to an over-estimation or under-estimation. Which evaluation metric you should use?
# Based on your choice of evaluation metric, which model has better performance, SVM or MLP?
# The Root Mean Squared Error (RMSE) as it gives a relatively high weigh to large errors, meannig it's more useful when large errors are problematic.
# The SVM performs better with a lower RMSE metric.

# Assume that the penalty for an erroneous prediction increases with the difference between the actual and predicted values. Which evaluation metric you should use?
# Based on your choice of evaluation metric, which model has better performance, SVM or MLP?
# The Mean Absolute Error (MAE) which averages the magnitdues of forecase errors with over-estimate or under-estimation. 
# The SVM performs better with a lower MAE metric.

