### -------------------------------------------------------------------------------
# You have been given a data file by the San Francisco Bay Area Rapid Transit (BART), 
# which identifies a set of demographics for residents in a local area. We will use 
# this file to determine residents segmentations so that we can use it to develop marketing
# plans accordingly.

# VARIABLE DESCRIPTIONS:
# Age:  1. 14 thru 17; 
#       2. 18 thru 24 
#       3. 25 thru 34
#       4. 35 thru 44
#       5. 45 thru 54
#       6. 55 thru 64
#       7. 65 and Over
# DistToWork: Distance to work in miles
# DualInc: Is dual income household or not
# Education:  1. Grade 8 or less
#             2. Grades 9 to 11
#             3. Graduated high school
#             4. 1 to 3 years of college
#             5. College graduate
#             6. Grad Study
# Gender:	M or F
# Income: 1. Less than $10,000
#         2. $10,000 to $14,999
#         3. $15,000 to $19,999
#         4. $20,000 to $24,999
#         5. $25,000 to $29,999
#         6. $30,000 to $39,999
#         7. $40,000 to $49,999
#         8. $50,000 to $74,999
#         9. $75,000 or more
# Language:	Language spoken at home
# NbrInHouseHold:	Number in household
# NbrInHouseholdUnder18:	Number in household under 18 years old
# OwnRent:	Own, rent, or live with parents
# YrsInArea:	Years lived in bay area
# Rider:	No, Non-rider; Yes, Rider
### ------------------------------------------------------------------------------


# 1. Import the datadet
BartRider <- read.csv(file = "BartRider.csv", stringsAsFactors = FALSE)

# 2.	Show the overall structure and summary of the input data.
str(BartRider)
summary(BartRider)

# 3.	Transform DualInc, Gender, Language, OwnRent, and Rider to factor variables. Show the overall structure and summary of the input data again.
BartRider$DualInc <- factor(BartRider$DualInc)
BartRider$Gender <- factor(BartRider$Gender)
BartRider$Language <- factor(BartRider$Language)
BartRider$OwnRent <- factor(BartRider$OwnRent)
BartRider$Rider <- factor(BartRider$Rider)
str(BartRider)
summary(BartRider)

# 4. Create dummy variables for factors
BartRider$DualInc <- ifelse(BartRider$DualInc == 'Y', 1, 0)
BartRider$Gender <- ifelse(BartRider$Gender == 'F', 1, 0)
BartRider$Language_English <- ifelse(BartRider$Language == 'English', 1, 0)
BartRider$Language_Spanish <- ifelse(BartRider$Language == 'Spanish', 1, 0)
BartRider$Language <- NULL
BartRider$OwnRent_own <- ifelse(BartRider$OwnRent == 'Own', 1, 0)
BartRider$OwnRent_Parent <- ifelse(BartRider$OwnRent == 'Parent', 1, 0)
BartRider$OwnRent <- NULL
BartRider$Rider <- ifelse(BartRider$Rider == 'Yes', 1, 0)
str(BartRider)
summary(BartRider)

# 5. k-mean clustering with number of cluster k = 2
library(RWeka)
BartRider_clustering <- SimpleKMeans(BartRider, Weka_control(N=2))

# show the results of 2 clusters
BartRider_clustering

# What is the size and centroids of each cluster?
# Cluster 0: 2678.0 
# Cluster 1: 2815.0

'Age                        3.4837     4.5732     2.4472
DistToWork                 11.4828    11.5362     11.432
DualInc                     0.2439     0.4671     0.0316
Education                   3.8724     4.4145     3.3567
Gender                      0.5385     0.6247     0.4565
Income                      5.1606     6.9712      3.438
NbrInHouseHold              2.9053     2.7054     3.0956
NbrInHouseholdUnder18       0.7073     0.5963     0.8128
YrsInArea                   4.2922     4.5063     4.0885
Rider                       0.4285     0.1617     0.6824
Language_English            0.9148     0.9563     0.8753
Language_Spanish            0.0553     0.0295     0.0799
OwnRent_own                 0.4353     0.8794     0.0128
OwnRent_Parent              0.2554          0     0.4984'

# What is the percentage of BART riders for cluster 0 and 1?
# Cluster 0: 0.1617 
# Cluster 1: 0.6824

# Use the attributes information of cluster centers to understand the profile of residents. Interpret each cluster based on your understanding.
# Cluster 0: Older, more income, own a house - likely own a car
# Cluster 1: Younger, less income, less likely to own home or car and thus more likely to be riders


# 6. k-mean clustering with number of cluster k = 3
BartRider_clustering3 <- SimpleKMeans(BartRider, Weka_control(N=3))

# show the results of 3 clusters
BartRider_clustering3

# What is the size and centroids of each cluster?
# Cluster 0: 2368.0
# Cluster 1: 1210.0
# Cluster 2: 1915.0

'Age                        3.4837     4.6917     3.2603     2.1311
DistToWork                 11.4828    11.5051    11.5231    11.4298
DualInc                     0.2439     0.4472     0.1694     0.0397
Education                   3.8724     4.4054     4.1347     3.0475
Gender                      0.5385     0.5904     0.4893     0.5055
Income                      5.1606     6.9949     5.6868     2.5598
NbrInHouseHold              2.9053     2.7758     2.2653       3.47
NbrInHouseholdUnder18       0.7073     0.6326     0.3702     1.0125
YrsInArea                   4.2922     4.5743     3.9256     4.1749
Rider                       0.4285     0.1858     0.0149     0.9901
Language_English            0.9148      0.951     0.9612     0.8407
Language_Spanish            0.0553     0.0329     0.0256     0.1018
OwnRent_own                 0.4353     0.9996          0     0.0125
OwnRent_Parent              0.2554          0     0.1157     0.6595'
# Assign a meaningful name to each cluster based on the representative profile of residents in each cluster. Explain the reasons for the name you choose.
# Cluster 0: Home Owners (.9996 Own, 0 live with parents)
# Cluster 1: Non-Home Owners (0 own)
# Cluster 2: Riders (.9901 are Riders, this cluster has the characteristics of riders)

# Based on the cluster size and centroids, which k (k=2 or k=3) you will use, and why?
# I would use k=3 as it provides us with a better view at Riders, with a .9901 result. Better distinction between clusters.

# If we segment residents into 3 clusters, what marketing plans you can use to target each cluster?
# Would focus more on the features of cluster 3 with a higher number of riders- Such as lower income, non-homeowners. 


# 7. elbow test/scree plot to determine optimal number of clusters.
# Step 1: initial a vector contains a list of k values from 1 to 10.
a_list_of_k = c(1:10)
# Step 2: initial a vector that contains errors
errors = vector()
# Step 3: create a for loop: for each k in the list of k (from 1 to 10)
# We run a simpleKMeans model based on that k and store the value in errors vector.
for (k in a_list_of_k) {
  model <- SimpleKMeans(BartRider, Weka_control(N=k))
  errors[k] = model$clusterer$getSquaredError()
  print(errors[k])
}
#Step 4: Plot the elbow plot: x axis = k and y axis = errors
plot(x = a_list_of_k, y = errors, xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares Error", type="b")


# What is the optimal cluster number?
# k = 3, it's the point of dimenishing returns.


# 8. Classification with clustering:

# Classfication without clustering
BartRider$Rider <- as.factor(BartRider$Rider)
str(BartRider)

# Testing performance for cross validation
library(matrixStats)
library(knitr)
library(C50)
library(rminer)
library(caret)
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

df <- BartRider
target <- 10
nFolds <- 10
seedVal <- 1
prediction_method <- C5.0
metrics_list <- c("ACC","PRECISION","TPR","F1")
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)


# Classfication with Clustering
BartRider_clustering3 <- SimpleKMeans(BartRider[,-10], Weka_control(N=3))
BartRider_clustering3
BartRider$class_ids = BartRider_clustering3$class_ids
BartRider1 = BartRider[which(BartRider$class_ids==0),]
BartRider2 = BartRider[which(BartRider$class_ids==1),]
BartRider3 = BartRider[which(BartRider$class_ids==2),]
str(BartRider1)
str(BartRider2)
str(BartRider3)

df <- BartRider1
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)
df <- BartRider2
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)
df <- BartRider3
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)

# Does the classification performance get better by applying clustering, why?
# Yes, it can provide better performance by allowing the model to make better predictions.

