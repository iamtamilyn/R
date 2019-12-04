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

# 7. Build MLP model with default setting
# MLP's default parameter values of MLP,L=0.3,M=0.2, N=500,H='a'
# L: learning rate with default=0.3
# M: momemtum with default=0.2
# N: number of epochs with default=500
# H <comma seperated numbers for nodes on each layer>
  #The hidden nodes to be created on each layer:
  # an integer, or the letters 'a' = (attribs + classes) / 2, 
  #'i' = attribs, 'o' = classes, 't' = attribs + classes)
  #for wildcard values, Default = 'a').
library(RWeka)
library(rminer)
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
mlp_model <- MLP(IsBadBuy~.,data=datTrain)
summary(mlp_model)

# Make predictions on both training and tessting sets
prediction_on_train <- predict(mlp_model, datTrain)
prediction_on_test <- predict(mlp_model, datTest)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain$IsBadBuy,prediction_on_train, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test,metric=c("ACC","PRECISION","TPR","F1"))


# 8. Build MLP model contains two hidden layers: 16 hidden nodes for the first layer, and 8 hidden nodes for the second layer. Set N = 100
mlp_model2 <-  MLP(IsBadBuy~.,data=datTrain, control = Weka_control(N=100, H='16, 8'))
summary(mlp_model2)

# Make predictions on both training and tessting sets
prediction_on_train2 <- predict(mlp_model2, datTrain)
prediction_on_test2 <- predict(mlp_model2, datTest)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain$IsBadBuy,prediction_on_train2, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test2, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train2,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test2,metric=c("ACC","PRECISION","TPR","F1"))


# 9. Build MLP model contains two hidden layers: 8 hidden nodes for the first layer, and 4 hidden nodes for the second layer. Set N = 100
mlp_model3 <-  MLP(IsBadBuy~.,data=datTrain, control = Weka_control(N=100, H='8, 4'))
summary(mlp_model3)

# Make predictions on both training and tessting sets
prediction_on_train3 <- predict(mlp_model3, datTrain)
prediction_on_test3 <- predict(mlp_model3, datTest)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain$IsBadBuy,prediction_on_train3, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test3, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train3,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test3,metric=c("ACC","PRECISION","TPR","F1"))

# 9. Build MLP model contains two hidden layers: 8 hidden nodes for the first layer, and 4 hidden nodes for the second layer. Set N = 100 and L=0.01
mlp_model4 <-  MLP(IsBadBuy~.,data=datTrain, control = Weka_control(L=0.01, N=100, H='8, 4'))
summary(mlp_model4)

# Make predictions on both training and tessting sets
prediction_on_train4 <- predict(mlp_model4, datTrain)
prediction_on_test4 <- predict(mlp_model4, datTest)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain$IsBadBuy,prediction_on_train4, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test4, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train4,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test4,metric=c("ACC","PRECISION","TPR","F1"))

# How dose the model complexity affects MLP model performance?
# Complex model (with more hidden layers and hidden nodes) fitts better on the training data.
# Very complex model may cause overfitting on training data.

# Which MLP model provides the best performance, and why?
# MLP model with L=0.01, N=100, H='8, 4' has the highest accuracy on testing data.
# MLP model with N=100, H='16, 8' provides highest TPR2 value on testing data.
# You can pick the model you like and provide explanations.

# 10. cross validation for MLP
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

# 3-fold cv with MLP method
df <- carAuction
target <- 3
nFolds <- 3
seedVal <- 1
prediction_method <- MLP
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

# 6. Build MLP model contains two hidden layers: 16 hidden nodes for the first layer , and 8 hidden nodes for the second layer. Set N = 100
mlp_model5 <- MLP(expenses~.,data=datTrain2, control = Weka_control(N=100, H='16, 8'))
summary(mlp_model5)

# Make predictions on both training and tessting sets
prediction_on_train5 <- predict(mlp_model5, datTrain2)
prediction_on_test5 <- predict(mlp_model5, datTest2)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain2$expenses,prediction_on_train5,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest2$expenses,prediction_on_test5,metric=c("MAE","RMSE","MAPE","RAE"))

# 7. Build MLP model contains two hidden layers: 8 hidden nodes for the first layer, and 4 hidden nodes for the second layer. Set N = 100
mlp_model6 <- MLP(expenses~.,data=datTrain2, control = Weka_control(N=100, H='8, 4'))
summary(mlp_model6)

# Make predictions on both training and tessting sets
prediction_on_train6 <- predict(mlp_model6, datTrain2)
prediction_on_test6 <- predict(mlp_model6, datTest2)

# Generating evaluation metrics on both training and testing data 
mmetric(datTrain2$expenses,prediction_on_train6,metric=c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest2$expenses,prediction_on_test6,metric=c("MAE","RMSE","MAPE","RAE"))

# Which MLP model you prefer, and why?
# The first model is better. It has lower MAE, MAPE, and RAE. The RMSE is slightly higher, indicating it generates more large errors, while, it is still very close to the RMSE of the second model. 

# 8. 3-fold cv on insurance data with MLP method
# Set up cv parameters
# df: identifies the whole data set by its name
# target: identifies the target variable by its column index in df
# nFolds: indicates the number of folds for cv
# seedVal: carries the seed value for random sampling of instances when creating folds
# prediction_method: indicates the prediction method - e.g., lm
# metric_list: is a list of evaluation metrics that mmetric should generate
df <- insurance
target <- 7
nFolds <- 3
seedVal <- 1
prediction_method <- MLP
metrics_list <- c("MAE","RMSE","MAPE","RAE")
cv_function_train(df, target, nFolds, seedVal, prediction_method, metrics_list)
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)

