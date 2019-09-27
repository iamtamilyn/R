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

  # b. What are the conditional probabilities of P(WheelType = unkwnWheel|IsBadBuy = Yes) and P(WheelType = unkwnWheel|IsBadBuy = No)?

  # c. For a new car X = (WheelType = unkwnWheel, Auction = OTHER, Color = GOLD), we can calculate
  # P(IsBadBuy = No|X) ∝ P(X|IsBadBuy = No) * P(IsBadBuy = No) = 
  # P(IsBadBuy = Yes|X) ∝ P(X|IsBadBuy = Yes) * P(IsBadBuy = Yes) = 
  # What is the prediction result based on your calculation?
  

# 8. Make predictions on both training and tessting sets
prediction_on_train <- predict(model, datTrain)
prediction_on_test <- predict(model, datTest)

# 9. Results comparison  
mmetric(datTrain$IsBadBuy,prediction_on_train, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test,metric=c("ACC","PRECISION","TPR","F1"))

  # a. Does the naive bayes model have better performance on training set or testing set? why?

  # b. Does the naive bayes model have better performance on majority (IsBadBuy = 'No') or minority class (IsBadBuy = 'Yes')? why?

  # c. How many bad buy cars are identified by the naive bayes model in testing data?

  # d. How many cars are predicted as bad buy in testing data? Again in the testing data, if the naive bayes predicts a car as bad buy, what is the probability that such prediction is correct?


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


# 14. Make predictions on both training and tessting sets


# 15. Compare the evaluation results on training and testing sets 


# 16. Build decision tree model with cp = 0.0001, maxdepth = 3.


# 17. Make predictions on both training and tessting sets


# 18. Compare the evaluation results on training and testing sets 


# 19. Build decision tree model with cp = 0.0001, maxdepth = 4.


# 20. Make predictions on both training and tessting sets


# 21. Compare the evaluation results on training and testing sets 



  #Compare the performances of decision tree model with different maxdepth
  # a. Does the decision tree model with maxdepth = 2 generalize well on the testing set? why?

  # b. If you are a car dealer, which decision tree model you will use, and why?




