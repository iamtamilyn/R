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

# 7. Build C50 models 
library(C50)
library(rminer)
model <- C5.0(IsBadBuy~.,data=datTrain)
model
plot(model)
summary(model)

# 8. Make predictions on both training and tessting sets
prediction_on_train <- predict(model, datTrain)
prediction_on_test <- predict(model, datTest)

# 9. Results comparison  
mmetric(datTrain$IsBadBuy,prediction_on_train, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train,c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test,metric=c("ACC","PRECISION","TPR","F1"))

# 10. Build C50 decision tree with CF = 0.5
model2 <- C5.0(IsBadBuy~.,data=datTrain, control = C5.0Control(CF = 0.5))
model2
plot(model2, type = 'simple')
summary(model2)

# 11. Make predictions on both training and tessting sets
prediction_on_train2 <- predict(model2, datTrain)
prediction_on_test2 <- predict(model2, datTest)

# 12. Compare the evaluation results on training and testing sets  
mmetric(datTrain$IsBadBuy,prediction_on_train2, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train2,c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test2, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test2,metric=c("ACC","PRECISION","TPR","F1"))

# a. Does the decision tree model have better performance on training set or testing set? why?
# The model has better performance on training data, because the overall accuracy, precision, recall, and F-measure are higher on training set.

# b. Does the decision tree model have better performance on majority (IsBadBuy = 'No') or minority class (IsBadBuy = 'Yes')? why?
# The model has better performance on majority class. It has much higher F-measure value (as well as higher precision and recall values) on majority class.


# 13. Build C50 decision tree with 8 predictors (removing WheelType and Auction) and set CF = 0.3
model3 <- C5.0(IsBadBuy~.,data=datTrain[,c(-1,-11)], control = C5.0Control(CF = 0.3))
model3
plot(model3)
summary(model3)

# a. How many decision nodes and how many leaf nodes are in the tree?
# 6 decision nodes, 8 leaf nodes

# b. Compare it to the C50 tree generated in task 7,  is it more or less complex? Give reasons for your answer.
# This model is more complex, because it has larger size (more leaf nodes) and more depth.

# c. What is the predictor that first splits the tree? How the decision tree selects the first predictor to split?
# The first predictor is VehBCost. It has the highest information gain among all predictors. The decision tree model select predictor based on the information gain.

# d. Find one path in the tree to a leaf node that is classified to IsBadBuy = 'Yes'. What is this path/rule's misclassification error rate?
# if VehBCost <= 4010, Color in {BROWN, PURPLE}, then IsBadBuy = 'Yes'. The error rate is 1/5 = 0.2

# 14. Make predictions on both training and tessting sets
prediction_on_train3 <- predict(model3, datTrain)
prediction_on_test3 <- predict(model3, datTest)

# 15. Compare the evaluation results on training and testing sets  
mmetric(datTrain$IsBadBuy,prediction_on_train3, metric="CONF")
mmetric(datTrain$IsBadBuy,prediction_on_train3,c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$IsBadBuy,prediction_on_test3, metric="CONF")
mmetric(datTest$IsBadBuy,prediction_on_test3,metric=c("ACC","PRECISION","TPR","F1"))

# a. On the testing set, how many bad buy cars are predicted as Not bad buy?
# 385

# b. Compared to the decision tree model generated in task 7, which one (model in task 7 or task 13) has better performance, and why?
# The model in task 7 has better performance. It has higher overall accuracy on the testing set, and the F-measure (on the testing set) on both classes are higher as well.


