##### IS 4482 Homework 2-----------------------------------------------------------

### -------------------------------------------------------------------------------
#Banks can generate significant profits from term deposits such as a certificate of deposit (CD). 
#These deposits are required to be held for a certain period of time, which gives the bank access 
#to those funds for lending purposes at a higher rate than the rate paid for the deposit. Of course, 
#marketing term deposit products to customers can be expensive, so the bank will want to focus their 
#efforts on those customers most likely to buy these products.

#In this data set, we have information about 45,211 customers, including demographic information as 
#well as data related to their prior experience with the bank and previous marketing campaigns.  
#Additionally, we have a class variable "y" that indicates whether this customer purchased 
#a term product in the current marketing campaign.  Our objective is to predict which customers will 
#purchase a term product if we spend the money to advertise to them.  We want to develop a model that 
#will maximize the returns based on the costs of marketing and the benefits of customer purchase. 
#This data was from a paper published by Moro et al. (S. Moro, P. Cortez and P. Rita. A Data-Driven 
#Approach to Predict the Success of Bank Telemarketing. Decision Support Systems, Elsevier, 62:22-31, June 2014)


'VARIABLE DESCRIPTIONS:
1 - age (numeric)
2 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
                       "blue-collar","self-employed","retired","technician","services") 
3 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
4 - education (categorical: "unknown","secondary","primary","tertiary")
5 - default: has credit in default? (categorical: "yes","no")
6 - balance: average yearly balance, in euros (numeric) 
7 - housing: has housing loan? (categorical: "yes","no")
8 - loan: has personal loan? (categorical: "yes","no")
9 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
10 - day: last contact day of the month (numeric)
11 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
12 - duration: last contact duration, in seconds (numeric)
13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
15 - previous: number of contacts performed before this campaign and for this client (numeric)
16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")

Target variable:
17 - y: has the client subscribed a term deposit? (binary: "yes","no")

The target classification (output) column is y.  All other columns are potential predictors.'
### ------------------------------------------------------------------------------

setwd("C:\\git\\repo\\R\\Data Mining")
### 1. Import and clean data (10 points)
# A. Import data. Load character variable as character strings first (stringsAsFactors = FALSE).
bank <- read.csv(file = "bank.csv", stringsAsFactors = FALSE)

# B.	Show the overall structure and summary of the input data.
str(bank)
summary(bank)

# C.	Transform categorical variables (10 categorical variables) to factors. Show the overall structure and summary of the data again.
bank$job <- factor(bank$job)
bank$marital <- factor(bank$marital)
bank$education <- factor(bank$education)
bank$default <- factor(bank$default)
bank$housing <- factor(bank$housing)
bank$loan <- factor(bank$loan)
bank$contact <- factor(bank$contact)
bank$month <- factor(bank$month)
bank$poutcome <- factor(bank$poutcome)
bank$y <- factor(bank$y)

str(bank)
summary(bank)

# D. Explore categorical variables, and answer the following questions.
# 1) Show the distribution of target variable.   
summary(bank$y)

# 2) How many customers have housing loans? 
summary(bank$housing[bank$housing =="yes"])[2]

# 3) How many customers are retired?
summary(bank$job[bank$job =="retired"])[6]

# E. Explore numeric variables, and answer the following questions.
# 1) Create a histogram of the balance. Is the distribution skewed?
hist(bank$balance, main = "Histogram of Balance", xlab = "Balance")
print("Yes, the distribution is skewed.")

# 2) Create a correlation table for all of the numeric values in the data set. Which two variables have the highest correlation?
cor(bank[,sapply(bank, is.numeric)])
print("The correlation between balance and age is the highest correlation of 0.097782739")

# 3) Show a boxplot of duration by variable y.
boxplot(duration~y, data = bank)
 

### 2. Data partitioning and inspection code (10 points)
# A.	Partition the data set for simple hold-out evaluation - 70% for training and the other 30% for testing.
library(caret)
set.seed(1)
train_index <- createDataPartition(bank$y, p=0.7, list=FALSE)
datTrain = bank[train_index,]
datTest = bank[-train_index,]

# B.	Show the overall structure and summary of train and test sets. Show the distributions of variable y in the entire set, the train set and the test set.
nrow(datTrain)
nrow(datTest)
prop.table(table(bank$y))
prop.table(table(datTrain$y))
prop.table(table(datTest$y))


### 3. Classification model training and testing. (10 points)
# A.	Train a decision model using rpart package, set cp = 0.0001, maxdepth = 5.  
# Generate this model's confusion matrices and classification evaluation metrics in training and testing sets.
library(rpart)
library(rpart.plot)
library(rminer)
rpart_model <- rpart(y~.,data = bank,control = rpart.control(cp = 0.0001, maxdepth = 5))
rpart.plot(rpart_model)
rpart_model

prediction_on_train_dtree <- predict(rpart_model, bank)
prediction_on_test_dtree <- predict(rpart_model, bank)

mmetric(bank$y,prediction_on_train_dtree, metric="CONF")
mmetric(bank$y,prediction_on_train_dtree,c("ACC","PRECISION","TPR","F1"))
mmetric(bank$y,prediction_on_test_dtree, metric="CONF")
mmetric(bank$y,prediction_on_test_dtree,metric=c("ACC","PRECISION","TPR","F1"))

# B.	Train a Naive Bayes model with laplace = 1.  
# Generate this model's confusion matrices and classification evaluation metrics in training and testing sets.
library(e1071)
naive_model <- naiveBayes(y~.,data=bank, laplace = 1)
naive_model

prediction_on_train_nb <- predict(naive_model, bank)
prediction_on_test_nb <- predict(naive_model, bank)

mmetric(bank$y,prediction_on_train_nb, metric="CONF")
mmetric(bank$y,prediction_on_train_nb,c("ACC","PRECISION","TPR","F1"))
mmetric(bank$y,prediction_on_test_nb, metric="CONF")
mmetric(bank$y,prediction_on_test_nb,metric=c("ACC","PRECISION","TPR","F1"))

# C. If you are a manager, which model (decision tree or naive bayes) you will use? And why?



### 4. Cost-benefit analysis. (15 points)
# A. Assume the following costs / benefits: 
  # a.	Cost of marketing to a customer: $50 per customer receiving marketing.
  # b.	Average Bank income for a purchase: $500 per customer that purchases term deposit.
  # c.	Opportunity cost of person not marketed but who would have been a purchaser: $500
# Based on the above costs / benefits, what is the total net benefit / cost of the two models (decision tree and naive bayes) on testing data? 


#	B. Create a cost matrix that sets the cost of classifying a purchaser of a term deposit as a non-purchaser to be 5 times the cost of the opposite (that is classifying a non-purchaser as a purchaser).
matrix_dimensions <- list(c("No", "Yes"),c("predict_No", "Predict_Yes"))
costMatrix<-matrix(c(0,5,1,0),nrow=2,dimnames = matrix_dimensions)
print(costMatrix)

# C. Use this cost matrix to build a decision tree model, and set cp = 0.0001, maxdepth = 5.
# Generate this model's confusion matrices and classification evaluation metrics in training and testing sets.
rpart_model_cm <- rpart(y~.,data = bank,control = rpart.control(cp = 0.0001, maxdepth = 5), parms = list(loss = costMatrix))
rpart.plot(rpart_model_cm)
rpart_model_cm

prediction_on_train_cm <- predict(rpart_model_cm, datTrain, type = "class")
prediction_on_test_cm <- predict(rpart_model_cm, datTest, type = "class")

mmetric(bank$y,prediction_on_train_cm, metric="CONF")
mmetric(bank$y,prediction_on_train_cm, metric="CONF")
mmetric(bank$y,prediction_on_test_cm,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(bank$y,prediction_on_test_cm,metric=c("ACC","PRECISION","TPR","F1"))

# D. Based on the costs / benefits, what is the total net benefit / cost of this new decision tree model on testing data?



# E. Compare decision tree models with or without cost matrix. By incorporating the cost matrix, which evaluation metrics get improved? What are the benefits to have higher values on these evaluation metrics?



### 5. Cross-validation with decision tree model. (5 points)
# Load packages for cross-validation
library(matrixStats)
library(knitr)

# Cross-validation function for training performances
cv_tree_train <- function(df, target, nFolds, seedVal, metrics_list, maxdepth)
{
  # create folds using the assigned values
  set.seed(seedVal)
  folds = createFolds(df[,target],nFolds)
  # The lapply loop
  cv_results <- lapply(folds, function(x)
  { 
    # data preparation:
    test_target <- df[x,target]
    test_input <- df[x,-target]
    train_target <- df[-x,target]
    train_input <- df[-x,-target]
    pred_model <- rpart(train_target~.,data = train_input,control = rpart.control(cp = 0.0001, maxdepth = maxdepth))
    pred_train <- predict(pred_model, train_input)
    return(mmetric(train_target,pred_train,metrics_list))
  })
  # convert a list to a data frame using as.data.frame and convert this data frame to a matrix before using rowSds()
  cv_results_m <- as.matrix(as.data.frame(cv_results))
  cv_mean<- as.matrix(rowMeans(cv_results_m))
  cv_sd <- as.matrix(rowSds(cv_results_m))
  colnames(cv_mean) <- "Mean"
  colnames(cv_sd) <- "Sd"
  kable(t(cbind(cv_mean,cv_sd)),digits=3)
}

# Cross-validation function for testing performances
cv_tree_test <- function(df, target, nFolds, seedVal, metrics_list, maxdepth)
{
  # create folds using the assigned values
  set.seed(seedVal)
  folds = createFolds(df[,target],nFolds)
  # The lapply loop
  cv_results <- lapply(folds, function(x)
  { 
    # data preparation:
    test_target <- df[x,target]
    test_input <- df[x,-target]
    train_target <- df[-x,target]
    train_input <- df[-x,-target]
    pred_model <- rpart(train_target~.,data = train_input,control = rpart.control(cp = 0.0001, maxdepth = maxdepth))  
    pred <- predict(pred_model, test_input)
    return(mmetric(test_target,pred,metrics_list))
  })
  cv_results_m <- as.matrix(as.data.frame(cv_results))
  cv_mean<- as.matrix(rowMeans(cv_results_m))
  cv_sd <- as.matrix(rowSds(cv_results_m))
  colnames(cv_mean) <- "Mean"
  colnames(cv_sd) <- "Sd"
  kable(t(cbind(cv_mean,cv_sd)),digits=3)
}

# A. Generate the 10 fold cross-validation results (for both training and testing data) by utilizing the two cross-validation functions. Set the tree maxdepth = 5.
df = bank
target = 17
nFolds = 10
seedVal = 1
metrics_list = c("ACC","PRECISION","TPR","F1")
maxdepth = 5
# Evaluation results on training data
cv_tree_train(df, target, nFolds, seedVal, metrics_list, maxdepth)
# Evaluation results on testing data
cv_tree_test(df, target, nFolds, seedVal, metrics_list, maxdepth)


##### end-------------------------------------------------------------------------