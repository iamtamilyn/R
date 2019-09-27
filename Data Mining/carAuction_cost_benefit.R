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

# 5. Partition the dataset for Decision Tree model   
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

# 7. Build rpart models 
library(rpart)
library(rpart.plot)
library(rminer)
rpart_model <- rpart(IsBadBuy~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 3))
rpart.plot(rpart_model)
rpart_model

# 8. Make predictions on both training and tessting sets
prediction_on_train <- predict(rpart_model, datTrain)
prediction_on_test <- predict(rpart_model, datTest)

# 9. Results comparison  
mmetric(datTrain$IsBadBuy,prediction_on_train, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test,metric=c("ACC","PRECISION","TPR","F1"))

# 10. Cost matrix
matrix_dimensions <- list(c("No", "Yes"),c("predict_No", "Predict_Yes"))
costMatrix<-matrix(c(0,5,1,0),nrow=2,dimnames = matrix_dimensions)
print(costMatrix)

# 11. Build rpart cost model 
library(rpart)
library(rpart.plot)
library(rminer)
rpart_model2 <- rpart(IsBadBuy~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 3), parms = list(loss = costMatrix))
rpart.plot(rpart_model2)
rpart_model2

# 8. Make predictions on both training and tessting sets
prediction_on_train2 <- predict(rpart_model2, datTrain, type = "class")
prediction_on_test2 <- predict(rpart_model2, datTest, type = "class")

# 9. Results comparison  
mmetric(datTrain$IsBadBuy,prediction_on_train2, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test2, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train2,metric=c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test2,metric=c("ACC","PRECISION","TPR","F1"))

# Notes
# The cost matrix version is less accurate, but saves us money. 
# We predict more cars as bad buys, like safe bets in order to avoid costly buys. 
# It's a game of balance.
