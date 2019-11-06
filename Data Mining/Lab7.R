##Part 1. Classification with CarAuction Data

### ---------------------------------------------------------------------------
# This dataset contains information of cars purchased at the Auction.
# We will use this file to predict the quality of buying decisions and visualize decision processes.

# VARIABLE DESCRIPTIONS:
#Auction: Auction provider at which the  vehicle was purchased
#Color: Vehicle Color
#IsBadBuy: Identifies if the kicked vehicle was an avoidable purchase
#MMRCurrentAuctionAveragePrice: Acquisition price for this vehicle in average condition as of current day
#Size: The size category of the vehicle (Compact, SUV, etc.)
#TopThreeAmericanName:Identifies if the manufacturer is one of the top three American manufacturers
#VehBCost: Acquisition cost paid for the vehicle at time of purchase
#VehicleAge: The Years elapsed since the manufacturer's year
#VehOdo: The vehicles odometer reading
#WarrantyCost: Warranty price (term=36month  and millage=36K)
#WheelType: The vehicle wheel type description (Alloy, Covers)
### ---------------------------------------------------------------------------


# 1. Import the datadet
carAuction <- read.csv(file = "carAuction.csv", stringsAsFactors = FALSE)

# 2. str() shows the structure of data
str(carAuction)

# 3. summary() shows the mean and the five-number statistics indicating the spread of each column's values
summary(carAuction)

# 4. Change all categorical variables to factors
carAuction$Auction <- factor(carAuction$Auction)
carAuction$Color <- factor(carAuction$Color)
carAuction$IsBadBuy <- factor(carAuction$IsBadBuy)
carAuction$Size <- factor(carAuction$Size)
carAuction$TopThreeAmericanName <- factor(carAuction$TopThreeAmericanName)
carAuction$WheelType <- factor(carAuction$WheelType)
str(carAuction)
summary(carAuction)

# 5. Partition the dataset: 70% for training, 30% for testing 
library(caret)
set.seed(1)
train_index <- createDataPartition(carAuction$IsBadBuy, p=0.7, list=FALSE)
datTrain <- carAuction[train_index,]
datTest <- carAuction[-train_index,]

# 6. Check the rows and porportion of target variable for both training and testing datasets
nrow(datTrain)
nrow(datTest)
prop.table(table(datTrain$IsBadBuy))
prop.table(table(datTest$IsBadBuy))

# 7. Build svm model with default setting (in default setting, C=1)
library(kernlab)
library(rminer)
svm_model <- ksvm(IsBadBuy~.,data=datTrain)
svm_model

# Make predictions on both training and tessting sets
prediction_on_train <- predict(svm_model, datTrain)
prediction_on_test <- predict(svm_model, datTest)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain$IsBadBuy,prediction_on_train, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test,metric=c("ACC","PRECISION","TPR","F1"))


# 8. Build svm model with C=5
svm_model2 <- ksvm(IsBadBuy~.,data=datTrain, C=5)
svm_model2

# Make predictions on both training and tessting sets
prediction_on_train2 <- predict(svm_model2, datTrain)
prediction_on_test2 <- predict(svm_model2, datTest)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain$IsBadBuy,prediction_on_train2, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test2, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train2,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test2,metric=c("ACC","PRECISION","TPR","F1"))


# 8. Build svm model with C=10
svm_model3 <- ksvm(IsBadBuy~.,data=datTrain, C=10)
svm_model3

# Make predictions on both training and tessting sets
prediction_on_train3 <- predict(svm_model3, datTrain)
prediction_on_test3 <- predict(svm_model3, datTest)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain$IsBadBuy,prediction_on_train3, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test3, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train3,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test3,metric=c("ACC","PRECISION","TPR","F1"))

# 9. Build svm model with C=50
svm_model4 <- ksvm(IsBadBuy~.,data=datTrain, C=50)
svm_model4

# Make predictions on both training and tessting sets
prediction_on_train4 <- predict(svm_model4, datTrain)
prediction_on_test4 <- predict(svm_model4, datTest)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain$IsBadBuy,prediction_on_train4, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test4, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train4,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test4,metric=c("ACC","PRECISION","TPR","F1"))

# How dose the cost parameter C impact SVM model performance?
# The accuracy starts to overrit to the training model and the performance on the testing data set declines.

# Which C value provides the best overall performance, C=1, 5, 10 or 50? And way?
# c=5 has the higest accuracy on the testing dataset. the higher c values have a decreased accuracy value.


#kernel:
#rbfdot: Radial Basis kernel "Gaussian"
#polydot: Polynomial kernel
#vanilladot: Linear kernel
#tanhdot: Hyperbolic tangent kernel
#laplacedot: Laplacian kernel
#besseldot: Bessel kernel
#anovadot: ANOVA RBF kernel
#splinedot: Spline kernel
#stringdot: String kernel

# 10. Build svm models with C=5, kernel = "vanilladot"
svm_model5 <- ksvm(IsBadBuy~.,data=datTrain, C=5, kernel = "vanilladot")
svm_model5

# Make predictions on both training and tessting sets
prediction_on_train5 <- predict(svm_model5, datTrain)
prediction_on_test5 <- predict(svm_model5, datTest)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain$IsBadBuy,prediction_on_train5, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test5, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train5,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test5,metric=c("ACC","PRECISION","TPR","F1"))


# 11. cross validation for SVM
# Set up cv parameters
# df: identifies the whole data set by its name
# target: identifies the target variable by its column index in df
# nFolds: indicates the number of folds for cv
# seedVal: carries the seed value for random sampling of instances when creating folds
# prediction_method: indicates the prediction method - e.g., lm
# metric_list: is a list of evaluation metrics that mmetric should generate
library(matrixStats)
library(knitr)
# Training performance for cross validation
cv_function_train <- function(df, target, nFolds, seedVal, prediction_method, metrics_list)
{
  # create folds
  set.seed(seedVal)
  folds = createFolds(df[,target],nFolds) 
  # perform cross validation
  cv_results <- lapply(folds, function(x)
  { 
    test_target <- df[x,target]
    test_input  <- df[x,-target]
    
    train_target <- df[-x,target]
    train_input <- df[-x,-target]
    
    prediction_model <- prediction_method(train_target~.,train_input) 
    pred<- predict(prediction_model,train_input)
    return(mmetric(train_target,pred,metrics_list))
  })
  # generate means and sds and show cv results, means and sds using kable
  cv_results_m <- as.matrix(as.data.frame(cv_results))
  cv_mean<- as.matrix(rowMeans(cv_results_m))
  cv_sd <- as.matrix(rowSds(cv_results_m))
  colnames(cv_mean) <- "Mean"
  colnames(cv_sd) <- "Sd"
  cv_all <- cbind(cv_results_m, cv_mean, cv_sd)
  kable(t(cv_all),digits=2)
}
# Testing performance for cross validation
cv_function_test <- function(df, target, nFolds, seedVal, prediction_method, metrics_list)
{
  # create folds
  set.seed(seedVal)
  folds = createFolds(df[,target],nFolds) 
  # perform cross validation
  cv_results <- lapply(folds, function(x)
  { 
    test_target <- df[x,target]
    test_input  <- df[x,-target]
    
    train_target <- df[-x,target]
    train_input <- df[-x,-target]
    
    prediction_model <- prediction_method(train_target~.,train_input) 
    pred<- predict(prediction_model,test_input)
    return(mmetric(test_target,pred,metrics_list))
  })
  # generate means and sds and show cv results, means and sds using kable
  cv_results_m <- as.matrix(as.data.frame(cv_results))
  cv_mean<- as.matrix(rowMeans(cv_results_m))
  cv_sd <- as.matrix(rowSds(cv_results_m))
  colnames(cv_mean) <- "Mean"
  colnames(cv_sd) <- "Sd"
  cv_all <- cbind(cv_results_m, cv_mean, cv_sd)
  kable(t(cv_all),digits=2)
}

# 3-fold cv with SVM method
df = carAuction
target = 3
nFolds = 3
seedVal = 1
prediction_method <- ksvm
metrics_list <- c("ACC","PRECISION","TPR","F1")

cv_function_train(df, target, nFolds, seedVal, prediction_method, metrics_list)
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)


##Part 2. Numeric Prediction with Insurance Data

'In order for a health insurance company to make money, it needs to collect more
in yearly premiums than it spends on medical care to its beneficiaries. As a result, 
insurers invest a great deal of time and money in developing models that accurately 
forecast medical expenses for the insured population.

Medical expenses are difficult to estimate because the most costly conditions are 
rare and seemingly random. Still, some conditions are more prevalent for certain 
segments of the population. For instance, lung cancer is more likely among smokers 
than non-smokers, and heart disease may be more likely among the obese.

The goal of this analysis is to use patient data to estimate the average medical
care expenses for such population segments. These estimates can be used to create 
actuarial tables that set the price of yearly premiums higher or lower, 
depending on the expected treatment costs.'

### ---------------------------------------------------------------------------
# The insurance data set has 1338 observations of 7 variables.
# We will use this file to predict the medical expenses.

# VARIABLE DESCRIPTIONS:
#age:	      age in years
#sex:	      gender
#bmi:	      body mass index
#children:	how many children do they have?
#smoker:	  do they smoke?
#region:	  geographic region
#expenses:	yearly medical expenses
### ---------------------------------------------------------------------------

# 1. Import the datadet
insurance <- read.csv(file = "insurance.csv", stringsAsFactors = FALSE)

# 2. str() shows the structure of data
str(insurance)

# 3. summary() shows the mean and the five-number statistics indicating the spread of each column's values
summary(insurance)

# 4. Change all categorical variables to factors
insurance$sex <- factor(insurance$sex)
insurance$smoker <- factor(insurance$smoker)
insurance$region <- factor(insurance$region)
str(insurance)
summary(insurance)

# 5. Partition the dataset: 70% for training, 30% for testing   
library(caret)
set.seed(1)
train_index <- createDataPartition(insurance$expenses, p=0.7, list=FALSE)
datTrain2 <- insurance[train_index,]
datTest2 <- insurance[-train_index,]

# 6. Build svm models with default setting (in default setting, C=1)
svm_model_i <- ksvm(expenses~.,data=datTrain2)
svm_model_i 

# Make predictions on both training and tessting sets
prediction_on_train_i <- predict(svm_model_i, datTrain2)
prediction_on_test_i <- predict(svm_model_i, datTest2)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain2$expenses,prediction_on_train_i,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest2$expenses,prediction_on_test_i,metric=c("MAE","RMSE","MAPE","RAE"))


# 7. Build svm model with C=5
svm_model_i2 <- ksvm(expenses~.,data=datTrain2, C=5)
svm_model_i2

# Make predictions on both training and tessting sets
prediction_on_train_i2 <- predict(svm_model_i2, datTrain2)
prediction_on_test_i2 <- predict(svm_model_i2, datTest2)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain2$expenses,prediction_on_train_i2,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest2$expenses,prediction_on_test_i2,metric=c("MAE","RMSE","MAPE","RAE"))

# 8. Build svm model with C=10
svm_model_i3 <- ksvm(expenses~.,data=datTrain2, C=10)
svm_model_i3 

# Make predictions on both training and tessting sets
prediction_on_train_i3 <- predict(svm_model_i3, datTrain2)
prediction_on_test_i3 <- predict(svm_model_i3, datTest2)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain2$expenses,prediction_on_train_i3,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest2$expenses,prediction_on_test_i3,metric=c("MAE","RMSE","MAPE","RAE"))


# Which C value provides the best performance?
# C = 5 

# How is SVM performed compared to mean estimator?
# Compare to the mean estimator our model is 

# Which evaluation metric tells you the percentage error?
# MAPE - provides the Mean Aboslute Percentage Error.

# Assume that you will lose each dollar your models prediction misses due to an over-estimation or under-estimation. Which evaluation metric you should use?
# MAE - Mean Average Error

# Assume that the penalty for an erroneous prediction increases with the difference between the actual and predicted values. Which evaluation metric you should use?
# RMSE - Root Mean Squared Error, it has higher weight for big errors. 

# 8. 3-fold cv on insurance data with SVM method
# Set up cv parameters
# df: identifies the whole data set by its name
# target: identifies the target variable by its column index in df
# nFolds: indicates the number of folds for cv
# seedVal: carries the seed value for random sampling of instances when creating folds
# prediction_method: indicates the prediction method - e.g., lm
# metric_list: is a list of evaluation metrics that mmetric should generate

df = insurance
target = 7
nFolds = 3
seedVal = 1
prediction_method <- ksvm
metrics_list <- c("MAE","RMSE","MAPE","RAE")

cv_function_train(df, target, nFolds, seedVal, prediction_method, metrics_list)
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)

