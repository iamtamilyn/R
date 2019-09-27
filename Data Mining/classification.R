# load library
library(RWeka)
library(partykit)
library(C50)

# import training and testing data
LawFirm = read.csv("LawFirm-Dateset.csv")
str(LawFirm)
Radio = read.csv("Radio_profile.csv")
str(Radio)

#clean - preprocess
LawFirm = LawFirm[,c(-1,-2,-5,-6,-7)]
summary(LawFirm)
Radio = Radio[,c(-1,-7)]
summary(Radio)

# transform: dob to age - transformatino
# install.packages("eeptools")
library(eeptools)
LawFirm$age = age_calc(as.Date(LawFirm$DOB, format = "%m/%d/%Y"), enddate = Sys.Date(), units = "years")
LawFirm = LawFirm[-1]
str(LawFirm)

# finding common - selection of attributes
LawFirm = LawFirm[,-3]
colnames(LawFirm)[colnames(LawFirm)=="education.backgroud"] <- "education.background"
levels(LawFirm[,2]) <- c(levels(LawFirm[,2]),"college degree")
str(LawFirm)
Radio[,4] = substr(Radio[,4],1,nchar(as.character(Radio[,4]))-1)
Radio[,4] = as.numeric(Radio[,4]) * 1000
colnames(Radio)[colnames(Radio)=="Gender"] <- "gender"
str(Radio)


# Partition the dataset for Decision Tree model   
library(caret)
set.seed(1)
train_index <- createDataPartition(LawFirm$LegalServiceRequested, p=0.7, list=FALSE)
datTrain <- LawFirm[train_index,]
datTest <- LawFirm[-train_index,]


# Train decision tree on law firm dataset
library(C50)
model=C5.0(LegalServiceRequested ~ ., data=datTrain, control =  C5.0Control(CF = 0.5))
plot(model)
model

# Make predictions on both training and tessting sets
prediction_on_train <- predict(model, datTrain,type = "class")
prediction_on_test <- predict(model, datTest,type = "class")

# Results comparison  
cm_train = as.matrix(table(Actual = datTrain$LegalServiceRequested, Predicted = prediction_on_train))
cm_train
cm_test = as.matrix(table(Actual = datTest$LegalServiceRequested, Predicted = prediction_on_test))
cm_test
# training performance
n = sum(cm_train) # number of instances
nc = nrow(cm_train) # number of classes
diag = diag(cm_train) # number of correctly classified instances per class 
rowsums = apply(cm_train, 1, sum) # number of instances per class
colsums = apply(cm_train, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)
#testing performance
n = sum(cm_test) # number of instances
nc = nrow(cm_test) # number of classes
diag = diag(cm_test) # number of correctly classified instances per class 
rowsums = apply(cm_test, 1, sum) # number of instances per class
colsums = apply(cm_test, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)




# Train the decision tree model on the whole LawFirm data
model_LawFirm=C5.0(LegalServiceRequested ~ ., data=LawFirm, control =  C5.0Control(CF = 0.5))

# Predict legal service type for radio listeners profile
predicted_radios = predict(model_LawFirm, newdata = Radio, type = "class")
output_data = read.csv("Radio_profile.csv")
output_data$radio = predicted_radios
View(output_data)

# Make decision based on the predicted probability
predicted_radios = predict(model_LawFirm, newdata = Radio, type = "prob")
predicted_radios


