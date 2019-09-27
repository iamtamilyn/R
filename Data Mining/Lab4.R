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
  print("We get 0.8704471 for No and 0.1295529 for Yes.")
  # b. What are the conditional probabilities of P(WheelType = unkwnWheel|IsBadBuy = Yes) and P(WheelType = unkwnWheel|IsBadBuy = No)?
  print("We get 0.267837541 for unknwnWheel & yes, and then a 0.013938996 on unknownWheel & No.")
  # c. For a new car X = (WheelType = unkwnWheel, Auction = OTHER, Color = GOLD), we can calculate
  # P(IsBadBuy = No|X) ∝ P(X|IsBadBuy = No) * P(IsBadBuy = No) = 0.013938996 * 0.2443825 * 0.069067103 * 0.8704471
  print(0.013938996 * 0.2443825 * 0.069067103 * 0.8704471)
  # P(IsBadBuy = Yes|X) ∝ P(X|IsBadBuy = Yes) * P(IsBadBuy = Yes) = 0.267837541 * 0.2219780 * 0.081256771  * 0.1295529 
  print(0.267837541 * 0.2219780 * 0.081256771  * 0.1295529 )
  # What is the prediction result based on your calculation?
  print("Predicting that IsBadBuy would be equal to 'Yes' based on higher probability.")
  

# 8. Make predictions on both training and tessting sets
prediction_on_train <- predict(model, datTrain)
prediction_on_test <- predict(model, datTest)

# 9. Results comparison  
mmetric(datTrain$IsBadBuy,prediction_on_train, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test,metric=c("ACC","PRECISION","TPR","F1"))

  # a. Does the naive bayes model have better performance on training set or testing set? why?
  print("Training data set has a better performance. Because the model was built by and for the training data. This shows in each of the ACC, Precision, Recal and F values.")
  # b. Does the naive bayes model have better performance on majority (IsBadBuy = 'No') or minority class (IsBadBuy = 'Yes')? why?
  print("Majority class. On the test results, there is a 92% (F11) majority vs 33% (F12) minority result.")
  # c. How many bad buy cars are identified by the naive bayes model in testing data?
  print("There are 101 bad cars identified where the real result was a bad car.")
  # d. How many cars are predicted as bad buy in testing data? Again in the testing data, if the naive bayes predicts a car as bad buy, what is the probability that such prediction is correct?
  print("This test set had 222 cars predicted as a bad buy. The probability that the prediction is correct is the Precision2 value. ")

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
rpart_model4 <- rpart(IsBadBuy~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 3))
rpart.plot(rpart_model4)
rpart_model4

# 20. Make predictions on both training and tessting sets
prediction_on_train_rpart4 <- predict(rpart_model4, datTrain)
prediction_on_test_rpart4 <- predict(rpart_model4, datTest)

# 21. Compare the evaluation results on training and testing sets 
mmetric(datTrain$IsBadBuy,prediction_on_train_rpart4, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test_rpart4, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train_rpart4, metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test_rpart4, metric = c("ACC","PRECISION","TPR","F1"))


  #Compare the performances of decision tree model with different maxdepth
  # a. Does the decision tree model with maxdepth = 2 generalize well on the testing set? why?
  print("Generalizes. The measures between training and testing results are very similar, in most causes only the decimals differ. ")
  # b. If you are a car dealer, which decision tree model you will use, and why?
  print("The decision tree performed better than the naive bayes. I would pick the maxdepth 3, as the results improved with each node until 4 when they were nearly identicle with maxdepth 3. Since the extra complexity of 4 doesn't seem to provide additional value, I would settle at maxdepth 3.")

