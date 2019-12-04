##Data description

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

# 7. Build KNN model with caret package. Set k=1
library(rminer)
model <- knn3(IsBadBuy~., datTrain, k=1)

# 8. Make predictions on both training and tessting sets
prediction_on_train <- predict(model, datTrain)
prediction_on_test <- predict(model, datTest)

# 9. Results comparison  
mmetric(datTrain$IsBadBuy,prediction_on_train, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test,metric=c("ACC","PRECISION","TPR","F1"))

# Compare the performances on training and testing sets
  # a. Why we have perfect evaluation results on the training data?
  # Because we set k=1, so we use only one closest neighbor in training data to make prediction. 
  # And for each training instance, the single closest neighbor is the instance itself, therefore, we use each instance's own label (target variable value) to make prediction, which lead to perfect evaluation performance.

  # b. Does the KNN model with k = 1 generalize well on the testing set? why?
  # No. This KNN model has perfect performance (100% accuracy) on training data, and the accuracy values between training and testing data have a big gap, so the model is overfitting the training data.

# 10. Build KNN model with k=5
model2 <- knn3(IsBadBuy~., datTrain, k=5)

# 11. Make predictions on both training and tessting sets
prediction_on_train2 <- predict(model2, datTrain)
prediction_on_test2 <- predict(model2, datTest)

# 12. Results comparison  
mmetric(datTrain$IsBadBuy,prediction_on_train2, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test2, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train2,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test2,metric=c("ACC","PRECISION","TPR","F1"))

# 10. Build KNN model with k=10
model3 <- knn3(IsBadBuy~., datTrain, k=10)

# 11. Make predictions on both training and tessting sets
prediction_on_train3 <- predict(model3, datTrain)
prediction_on_test3 <- predict(model3, datTest)

# 12. Results comparison  
mmetric(datTrain$IsBadBuy,prediction_on_train3, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test3, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train3,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test3,metric=c("ACC","PRECISION","TPR","F1"))

  # a. Which KNN model is the best for identifying bad buy cars (k=1, 5 or 10)? and why?
  # KNN model with k=1 is the best for identifying bad buy cars, because it has the highest recall value on IsBadBuy=Yes class.

# 13. Define functions for cross validation 
# Load packages for cross validation
library(matrixStats)
library(knitr)

# cross validation function for training
cv_knn_train <- function(df, target, nFolds, seedVal, metrics_list, k)
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
    pred_model <- knn3(train_target ~ .,data = train_input, k=k)  
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

# cross validation function for testing
cv_knn_test <- function(df, target, nFolds, seedVal, metrics_list, k)
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
    pred_model <- knn3(train_target ~ .,data = train_input,k=k)  
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

# 14. Apply cross validation on carAuction data with nFolds = 3 and k = 5, and generate evaluation results on training and testing data.
df = carAuction
target = 3
nFolds = 3 
seedVal = 1
metrics_list = c("ACC","PRECISION","TPR","F1")
k = 5
# Evaluation results on training data
cv_knn_train(df, target, nFolds, seedVal, metrics_list, k)
# Evaluation results on testing data
cv_knn_test(df, target, nFolds, seedVal, metrics_list, k)

# Apply cross validation on carAuction data with nFolds = 10 and k = 5, and generate evaluation results on training and testing data.
df = carAuction
target = 3
nFolds = 10 
seedVal = 1
metrics_list = c("ACC","PRECISION","TPR","F1")
k = 5
cv_knn_train(df, target, nFolds, seedVal, metrics_list, k)
cv_knn_test(df, target, nFolds, seedVal, metrics_list, k)


