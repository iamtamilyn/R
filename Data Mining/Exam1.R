##### IS 4482 Exam 1---------------------------------------------------------------

#### churn Data--------------------------------------------------------------------

### -------------------------------------------------------------------------------
'Customer churn, also known as customer attrition, occurs when customers stop doing 
business with a company. The companies are interested in identifying customers who 
were at risk of churning, so they could proactively engage them with special offers 
instead of simply losing them.

The churn dataset consists information of 20,000 customers of a telecommunication company.
The target variable, LEAVE, is the last variable, and its values are LEAVE and STAY. 

VARIABLE DESCRIPTIONS:
COLLEGE : Is the customer college educated?
INCOME : Annual income
OVERAGE : Average overcharges per month
LEFTOVER : Average % leftover minutes per month
HOUSE : Value of dwelling (from census tract)
HANDSET_PRICE : Cost of phone
OVER_15MINS_CALLS_PER_MONTH : Average number of long (>15 mins) calls per month 
AVERAGE_CALL_DURATION : Average call duration
REPORTED_SATISFACTION : Reported level of satisfaction 
REPORTED_USAGE_LEVEL : Self-reported usage level 
CONSIDERING_CHANGE_OF_PLAN : Was customer considering changing his/her plan? 
LEAVE : whether customer left or stayed'
### ------------------------------------------------------------------------------

### 1. Import and clean data. (3 points)
# A. Import data. Load character variable as character strings first (stringsAsFactors = FALSE).
churn <- read.csv(file = "churn.csv", stringsAsFactors = FALSE)

# B.	Show the overall structure and summary of the input data.
str(churn)
summary(churn)

# C.	Transform COLLEGE, REPORTED_SATISFACTION, REPORTED_USAGE_LEVEL, CONSIDERING_CHANGE_OF_PLAN, and LEAVE to factors. Show the structure and summary of the data again.
churn$COLLEGE <- factor(churn$COLLEGE)
churn$REPORTED_SATISFACTION <- factor(churn$REPORTED_SATISFACTION)
churn$REPORTED_USAGE_LEVEL <- factor(churn$REPORTED_USAGE_LEVEL)
churn$CONSIDERING_CHANGE_OF_PLAN <- factor(churn$CONSIDERING_CHANGE_OF_PLAN)
churn$LEAVE <- factor(churn$LEAVE)

str(churn)
summary(churn)

### 2. Data partitioning. (2 points)
# A.	Partition the data set for simple hold-out evaluation - 70% for training and the other 30% for testing.
library(caret)
set.seed(1)
train_index <- createDataPartition(churn$LEAVE, p=0.7, list=FALSE)
datTrain <- churn[train_index,]
datTest <- churn[-train_index,]

nrow(datTrain)
nrow(datTest)
prop.table(table(datTrain$LEAVE))
prop.table(table(datTest$LEAVE))


### 3. Simple decision tree training and testing. (5 points)
# A.	Train a decision tree model with rpart package. Set cp = 0.0001, maxdepth = 5. 
library(rpart)
library(rpart.plot)
library(rminer)
rpart_model <- rpart(LEAVE~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 5))
rpart.plot(rpart_model)
rpart_model

# B.  Make predictions on both training and tessting sets
prediction_on_train_rpart <- predict(rpart_model, datTrain)
prediction_on_test_rpart <- predict(rpart_model, datTest)

# C.  Generate this model's confusion matrices and classification evaluation metrics in training and tessting sets.
mmetric(datTrain$LEAVE,prediction_on_train_rpart, metric="CONF")
mmetric(datTest$LEAVE,prediction_on_test_rpart, metric="CONF")
mmetric(datTrain$LEAVE,prediction_on_train_rpart,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$LEAVE,prediction_on_test_rpart,metric=c("ACC","PRECISION","TPR","F1"))

### 4. Naive Bayes for cross validation. (2 points)
# A.	Load packages for cross validation
library(e1071)
library(matrixStats)
library(knitr)

# B.	cross validation function for training
cv_nb_train <- function(df, target, nFolds, seedVal, metrics_list)
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
    pred_model <- naiveBayes(train_target ~ .,data = train_input, laplace = 1)  
    pred_train <- predict(pred_model, train_input)
    return(mmetric(train_target,pred_train,metrics_list))
  })
  # convert a list to a data frame using as.data.frame and convert this data frame to a matrix before using rowSds()
  cv_results_m <- as.matrix(as.data.frame(cv_results))
  cv_mean<- as.matrix(rowMeans(cv_results_m))
  cv_sd <- as.matrix(rowSds(cv_results_m))
  colnames(cv_mean) <- "Mean"
  colnames(cv_sd) <- "Sd"
  # Combine and show cv_results and Means and Sds
  cv_all <- cbind(cv_results_m, cv_mean, cv_sd)
  kable(t(cv_all),digits=3)
}

#  C.  cross validation function for testing
cv_nb_test <- function(df, target, nFolds, seedVal, metrics_list)
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
    pred_model <- naiveBayes(train_target ~ .,data = train_input, laplace = 1)  
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

#  D.  Apply 3-fold cross validation on churn data with nFolds = 3, and generate evaluation results on training and testing data.
df = churn
target = 12
nFolds = 3 
seedVal = 1
metrics_list = c("ACC","PRECISION","TPR","F1")

cv_nb_train(df, target, nFolds, seedVal, metrics_list)
cv_nb_test(df, target, nFolds, seedVal, metrics_list)


##### end-------------------------------------------------------------------------
