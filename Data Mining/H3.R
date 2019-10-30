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


# B. Generate this model's confusion matrices and classification evaluation metrics in training and testing sets.


### 4. Neural Network for classification (6 points)
# A. Build a MLP model with N=100, H='8, 4'
library(RWeka)
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")


# B. Generate this model's confusion matrices and classification evaluation metrics in training and testing sets.


  # Which model is better for identifying riders, SVM or MLP?





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


  # Why we have multiple Coefficients for Platform, Genre, and Rating variables?

  # How Critic_Score affects Global_Sales value? e.g., How Global_Sales changes if we increase Critic_Score by 1?

  # How Critic_Count affects Global_Sales value? e.g., How Global_Sales changes if we increase Critic_Count by 1?

  # Are User_Score and User_Count important for predicting Global_Sales, and why? hint: using the p-value to determine the importance.

  # What is the value of Multiple R-squared? Interpret the meaning of this Multiple R-squared value.


# B. Generate this model's evaluation metrics on both training and testing data.


  # How is Multiple Linera Regression performed compared to mean estimator?

  # Which evaluation metric tells you the percentage error? Interpret the meaning of percentage error value on testing data.


### 4. Model Tree for predicting Global_Sales value. (7 points)
# A. Build a Model Tree with M5P function.


# B. Generate this model's evaluation metrics on both training and testing data.


  # Compared to Multiple Linera Regression, does Model Tree have better performance, and why?


### 5. SVM for predicting Global_Sales value. (7 points)
# A. Build a SVM model with your choice of C value.


  # How you choose this C value to build the SVM model?


# B. Generate this model's evaluation metrics on both training and testing data.




### 6. Neural Network for predicting Global_Sales value. (9 points)
# A. Build a MLP model with with N=100, H='8, 8'


# B. Generate this model's evaluation metrics on both training and testing data.



# Assume that you will lose each dollar your models prediction misses due to an over-estimation or under-estimation. Which evaluation metric you should use?
# Based on your choice of evaluation metric, which model has better performance, SVM or MLP?



# Assume that the penalty for an erroneous prediction increases with the difference between the actual and predicted values. Which evaluation metric you should use?
# Based on your choice of evaluation metric, which model has better performance, SVM or MLP?




