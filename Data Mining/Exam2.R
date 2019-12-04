##### IS 4482 Exam 2---------------------------------------------------------------

#### Video Game Sales Data--------------------------------------------------------------------

### -------------------------------------------------------------------------------
# Video games are a billion-dollar business and have been for many years. In 2016, 
# the video game market in the United States was valued at 17.68 billion U.S. dollars. 
# That same year U.S. consumers were said to spend roughly double the amount on gaming 
# content, hardware and accessories. What is important is that the first generation 
# of gamers is now grown up with significant spending power; therefore, despite high 
# penetration rates among kids, video games can no longer be considered solely a child’s 
# play. In fact, it was found that video gaming is gaining on popularity among the seniors 
# in the United States.

# This data provides video sales information together with game scores and ratings.
# Our task is to predict game sales by given the information of each game.

# VARIABLE DESCRIPTIONS:
# Name: Game name
# Platform: Platform of the games release (i.e. PC,PS4, etc.)
# Genre: Genre of the game
# Global_Sales: Total worldwide sales (in millions)
# Critic_Score: Aggregate score compiled by Metacritic staff
# Critic_Count: The number of critics used in coming up with the Critic_score 
# User_Score: Score by Metacritic’s subscribers
# User_Count: Number of users who gave the user_score
# Rating: The ESRB ratings
### ------------------------------------------------------------------------------

### 1. Import and clean data
# A. Import data. Load character variable as character strings first (stringsAsFactors = FALSE).
Sales <- read.csv(file = "sales.csv", stringsAsFactors = FALSE)

# B.	Show the overall structure and summary of the input data.
str(Sales)
summary(Sales)

# C. Remove the Name column
Sales$Name <- NULL

# D. Transform Platform, Genre, and Rating to factor variables. Show the overall structure and summary of the input data again.
Sales$Platform <- factor(Sales$Platform)
Sales$Genre <- factor(Sales$Genre)
Sales$Rating <- factor(Sales$Rating)
str(Sales)
summary(Sales)

### 2. Data partitioning
# A.	Partition the data set for simple hold-out evaluation - 70% for training and the other 30% for testing. (1 points)
library(caret)
set.seed(1)
train_index <- createDataPartition(Sales$Global_Sales, p=0.7, list=FALSE)
datTrain <- Sales[train_index,]
datTest <- Sales[-train_index,]

### 3. SVM for predicting Global_Sales value. (2 points)
# A. Build a SVM model with C=5.
library(kernlab)
library(rminer)
svm_model <- ksvm(Global_Sales~.,data=datTrain, C=5)

# B. Generate this model's evaluation metrics on both training and testing data.
prediction_on_train <- predict(svm_model, datTrain)
prediction_on_test <- predict(svm_model, datTest)

mmetric(datTrain$Global_Sales,prediction_on_train,metric = c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest$Global_Sales,prediction_on_test,metric = c("MAE","RMSE","MAPE","RAE"))

### 4. Neural Network for predicting Global_Sales value. (2 points)
# A. Build a MLP model with with N=50, H='8, 4'
library(RWeka)
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
mlp_model <- MLP(Global_Sales~.,data=datTrain, control = Weka_control(N=50, H='8, 4'))
summary(mlp_model)

# B. Generate this model's evaluation metrics on both training and testing data.
prediction_on_train_mlp <- predict(mlp_model, datTrain)
prediction_on_test_mlp <- predict(mlp_model, datTest)

mmetric(datTrain$Global_Sales,prediction_on_train_mlp,metric = c("MAE","RMSE","MAPE","RAE"))
mmetric(datTest$Global_Sales,prediction_on_test_mlp,metric = c("MAE","RMSE","MAPE","RAE"))


### 4. SVM for cross validation. (2 points)
# A.	Load packages for cross validation
library(matrixStats)
library(knitr)

# B. Training performance for cross validation
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

# C. Testing performance for cross validation
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


# D. Apply 3-fold cross validation on Sales data with SVM model, and generate evaluation results on training and testing data.

df <- Sales
target <- 3
nFolds <- 3
seedVal <- 1
prediction_method <- ksvm
metrics_list <- c("MAE","RMSE","MAPE","RAE")
cv_function_train(df, target, nFolds, seedVal, prediction_method, metrics_list)
cv_function_test(df, target, nFolds, seedVal, prediction_method, metrics_list)


##### end-------------------------------------------------------------------------