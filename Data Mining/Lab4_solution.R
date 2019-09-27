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

# 7. Build NB models with laplace = 1
library(e1071)
library(rminer)
model <- naiveBayes(IsBadBuy~.,data=datTrain, laplace = 1)
model

  # a. What are the prior probabilities of IsBadBuy = No and IsBadBuy = Yes?
    # P(IsBadBuy = No) = 0.8704471 and P(IsBadBuy = Yes) = 0.1295529 

  # b. What are the conditional probabilities of P(WheelType = unkwnWheel|IsBadBuy = Yes) and P(WheelType = unkwnWheel|IsBadBuy = No)?
    # P(WheelType = unkwnWheel|IsBadBuy = Yes) = 0.267837541, P(WheelType = unkwnWheel|IsBadBuy = No) = 0.013938996

  # c. For a new car X = (WheelType = unkwnWheel, Auction = OTHER, Color = GOLD), we can calculate
    # P(IsBadBuy = No|X) ∝ P(X|IsBadBuy = No) * P(IsBadBuy = No) = 0.013938996 * 0.2443825 * 0.069067103 * 0.8704471 = 0.0002047931
    # P(IsBadBuy = Yes|X) ∝ P(X|IsBadBuy = Yes) * P(IsBadBuy = Yes) = 0.267837541 * 0.2219780 * 0.081256771 * 0.1295529 = 0.0006258757
    # What is the prediction result based on your calculation?
      #IsBadBuy = Yes



# 8. Make predictions on both training and tessting sets
prediction_on_train <- predict(model, datTrain)
prediction_on_test <- predict(model, datTest)

# 9. Results comparison  
mmetric(datTrain$IsBadBuy,prediction_on_train, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test,metric=c("ACC","PRECISION","TPR","F1"))

  # a. Does the naive bayes model have better performance on training set or testing set? why?
  # The model has better performance on training data, because the overall accuracy, precision, recall, and F-measure are higher on training set.

  # b. Does the naive bayes model have better performance on majority (IsBadBuy = 'No') or minority class (IsBadBuy = 'Yes')? why?
  # The model has better performance on majority class. It has higher F-measure value (as well as higher precision and recall) on majority class.

  # c. How many bad buy cars are identified by the naive bayes model in testing data?
  # 101

  # d. How many cars are predicted as bad buy in testing data? Again in the testing data, if the naive bayes predicts a car as bad buy, what is the probability that such prediction is correct?
  # 222, 45.49550% (PRECISION2 value)


# 10. Build decision tree model with cp = 0.0001, maxdepth = 1.
library(rpart)
library(rpart.plot)
rpart_model <- rpart(IsBadBuy~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 1))
rpart.plot(rpart_model)
rpart_model

# 11. Make predictions on both training and tessting sets
prediction_on_train_rpart <- predict(rpart_model, datTrain)
prediction_on_test_rpart <- predict(rpart_model, datTest)

# 12. Compare the evaluation results on training and testing sets 
mmetric(datTrain$IsBadBuy,prediction_on_train_rpart, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test_rpart, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train_rpart,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test_rpart,metric=c("ACC","PRECISION","TPR","F1"))


# 13. Build decision tree model with cp = 0.0001, maxdepth = 2.
rpart_model2 <- rpart(IsBadBuy~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 2))
rpart.plot(rpart_model2)
rpart_model2

# 14. Make predictions on both training and tessting sets
prediction_on_train_rpart2 <- predict(rpart_model2, datTrain)
prediction_on_test_rpart2 <- predict(rpart_model2, datTest)

# 15. Compare the evaluation results on training and testing sets 
mmetric(datTrain$IsBadBuy,prediction_on_train_rpart2, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test_rpart2, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train_rpart2,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test_rpart2,metric=c("ACC","PRECISION","TPR","F1"))

# 16. Build decision tree model with cp = 0.0001, maxdepth = 3.
rpart_model3 <- rpart(IsBadBuy~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 3))
rpart.plot(rpart_model3)
rpart_model3

# 17. Make predictions on both training and tessting sets
prediction_on_train_rpart3 <- predict(rpart_model3, datTrain)
prediction_on_test_rpart3 <- predict(rpart_model3, datTest)

# 18. Compare the evaluation results on training and testing sets 
mmetric(datTrain$IsBadBuy,prediction_on_train_rpart3, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test_rpart3, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train_rpart3,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test_rpart3,metric=c("ACC","PRECISION","TPR","F1"))

# 19. Build decision tree model with cp = 0.0001, maxdepth = 4.
rpart_model4 <- rpart(IsBadBuy~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 4))
rpart.plot(rpart_model4)
rpart_model4

# 20. Make predictions on both training and tessting sets
prediction_on_train_rpart4 <- predict(rpart_model4, datTrain)
prediction_on_test_rpart4 <- predict(rpart_model4, datTest)

# 21. Compare the evaluation results on training and testing sets 
mmetric(datTrain$IsBadBuy,prediction_on_train_rpart4, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test_rpart4, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train_rpart4,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test_rpart4,metric=c("ACC","PRECISION","TPR","F1"))


  #Compare the performances of decision tree model with different maxdepth
  # a. Does the decision tree model with maxdepth = 2 generalize well on the testing set? why?
  # Yes. For decision tree model with maxdepth = 2, we compare the accuracy values on the training and testing data; these two accuracy values are very close, which means the model generalizes well on the testing set.
  # Moreover, decision tree model with maxdepth = 2 has the lowest prediction error (compared to other decision tree models) on the test set. 
  # When maxdepth<2, the decision tree model is underfit, and when maxdepth>3, the decision tree model is overfit. So maxdepth=2 is the optimal tree complexity, which provides good generalization.

  # b. If you are a car dealer, which decision tree model you will use, and why?
  # I'll use the model with maxdepth = 1, cause it provides relative high overall performance (high accuracy) with very good ability to identify bad buy cars (high recall values for IsBadBuy=Yes class on testing data).
  # You can choose a different model and provide explanations to support your choice. 



