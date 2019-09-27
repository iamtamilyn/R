##### Pokemon Data ---------------------------------------------------------

# Part 1: Pokemon Data Exploration
### ---------------------------------------------------------------------------
# This dataset contains information on 800 Pokemon from six generations of Pokemon.
# VARIABLE DESCRIPTIONS:
#number: The entry number of the Pokemon
#name: The English name of the Pokemon
#type1: The Primary Type of the Pokemon
#type2: The Secondary Type of the Pokemon
#hp: The Base HP of the Pokemon
#attack: The Base Attack of the Pokemon
#defense: The Base Defense of the Pokemon
#sp.atk: The Base Special Attack of the Pokemon
#sp.def: The Base Special Defense of the Pokemon
#speed: The Base Speed of the Pokemon
#generation: The numbered generation which the Pokemon was first introduced
#legendary: Denotes if the Pokemon is legendary.
### ---------------------------------------------------------------------------

### 1. Import and clean data
setwd('C:\\Users\\tltam\\OneDrive\\IS 4482 Data Mining\\QuizExam')
# Import data from csv
pokemon <- read.csv(file = "pokemon.csv", stringsAsFactors = FALSE)

# Nice data visualization
colnames(pokemon)<-c("id","Name","Type.1","Type.2","HP","Attack","Defense","Sp.Atk","Sp.Def","Speed","Generation","Legendary")
Type.1<-c("Dragon","Steel","Flying","Psychic","Rock" ,"Fire","Electric" ,"Dark","Ghost" ,"Ground","Ice", "Water","Grass","Fighting", "Fairy" ,"Poison","Normal","Bug")
color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797","#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A","#A6B91A")
COL<-data.frame(Type.1,color)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(plotly)
library(radarchart)
library(fmsb)
merge(
  merge(pokemon %>% dplyr::group_by(Type.1) %>% dplyr::summarize(tot=n()),
        pokemon %>% dplyr::group_by(Type.1,Legendary) %>% dplyr::summarize(count=n()),by='Type.1'),
  COL, by='Type.1') %>% 
  ggplot(aes(x=reorder(Type.1,tot),y=count)) + 
  geom_bar(aes(fill=color,alpha=Legendary),color='white',size=.25,stat='identity') + 
  scale_fill_identity() + coord_flip() + ggthemes::theme_fivethirtyeight() + 
  ggtitle("Pokemon Distribution") + scale_alpha_discrete(range=c(.9,.6))
res<-data.frame(pokemon %>% dplyr::select(Type.1,HP, Attack, Defense, Sp.Atk, Sp.Def, Speed) %>% dplyr::group_by(Type.1) %>% dplyr::summarise_all(funs(mean)) %>% mutate(sumChars = HP + Attack + Defense + Sp.Atk + Sp.Def + Speed) %>% arrange(-sumChars))
res$color<-color
max<- ceiling(apply(res[,2:7], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,6)
par(mfrow=c(3,6))
par(mar=c(1,1,1,1))
for(i in 1:nrow(res)){
  curCol<-(col2rgb(as.character(res$color[i]))%>% as.integer())/255
  radarchart(rbind(max,min,res[i,2:7]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", caxislabels=seq(0,2000,5), cglwd=0.8, vlcex=0.8,
             title=as.character(res$Type.1[i]))
}

# str() shows the structure of data
str(pokemon)

# examine the number of rows and cols
nrow(pokemon)
ncol(pokemon)

# Show the head and tail rows of a data frame
head(pokemon)
pokemon[1:6,]
head(pokemon, n=1)
tail(pokemon)

# summary() shows the mean and the five-number statistics indicating the spread of each column's values
summary(pokemon)

# Remove unique identifiers (pokemon number and Name) from further analysis.
pokemon <- pokemon[,c(-1,-2)]

# Change categorical variables to factors
str(pokemon)
pokemon$Type.1 <- factor(pokemon$Type.1)
pokemon$Type.2 <- factor(pokemon$Type.2)
pokemon$Generation <- factor(pokemon$Generation)
pokemon$Legendary <- factor(pokemon$Legendary)
str(pokemon)
summary(pokemon)

# set missing values in Type.2 as none
levels(pokemon$Type.2)
levels(pokemon$Type.2)[1]
levels(pokemon$Type.2)[1] <- "none"
str(pokemon)
summary(pokemon)

### 2. understanding a single variable: numerical variables

# Show summary of one or more columns
summary(pokemon$Attack)
summary(pokemon[,c("Attack", "Defense")])
summary(pokemon$Sp..Atk + pokemon$Sp..Def)
which(pokemon$Sp..Atk + pokemon$Sp..Def == 340)

# obtain the mean, median, and range of a numeric variable
mean(pokemon$Attack)
median(pokemon$Attack)
range(pokemon$Attack)

# use quantile to calculate the five-number summary for Attack
quantile(pokemon$Attack)

# IQR
IQR(pokemon$Attack)

# boxplot of numeric variables
boxplot(pokemon$Attack, main="Boxplot of Attack in the pokemon data set", ylab="Attack")
boxplot(pokemon$Defense, main="Boxplot of Defense in the pokemon data set", ylab="Defense")
boxplot(pokemon[which(pokemon$Generation==1),4], main="Boxplot of Attack of the 1st generation pokemon", ylab="Attack")
boxplot(pokemon[which(pokemon$Generation==1),5], main="Boxplot of Defense of the 1st generation pokemon", ylab="Defense")

# histograms of a numeric variable
hist(pokemon$Attack, main = "Histogram of Attack in the pokemon data set", xlab = "Attack")
hist(pokemon$Defense, main = "Histogram of Defense in the pokemon data set", xlab = "Defense")
hist(pokemon$HP, main = "Histogram of HP in the pokemon data set", xlab = "HP")

# variance and standard deviation of a numeric varaible
var(pokemon$Attack)
sd(pokemon$Attack)


### 3. Exploring categorical variables

# Summary of categorical variable
summary(pokemon$Type.1)
nlevels(pokemon$Type.1)

# Plot categorical variable
plot(pokemon$Type.1, main = "Plot of Type.1 in the pokemon data set", xlab = "Type.1")
table(pokemon$Type.1)
sort(table(pokemon$Type.1))

# Run prop.table
Type_table = table(pokemon$Type.1)
prop.table(Type_table)


### 4. Understand relationships of multiple variables

# scatter plot: two numeric variables
plot(pokemon$Attack, pokemon$Defense)

# Generate correlation coefficients of two numeric variables in a 2x2 matrix
# cor(X,Y) lies between -1 and 1. zero means no correlation. 1 or -1 indicates full correlation
# positive value means positive correlation and negative values mean negative relationships
cor(pokemon[,c("Attack", "Defense")])
cor(pokemon[,c(4,5)])

# Generate the correlation matrix of all numeric variables
cor(pokemon[,3:8])

# Generate 2D scatter plots
pairs(pokemon[,3:8])

## Examine relationships between numeric variables and factors
# boxplot groups values of a numeric variable based on the values of a factor
boxplot(Attack~Type.1, data = pokemon)
boxplot(Attack~Type.1, data = pokemon[which(pokemon$Legendary=='True'),])
boxplot(Attack~Type.1, data = pokemon[which(pokemon$Legendary=='False'),])
boxplot(HP~Legendary, data = pokemon)
boxplot(HP~Type.1, data = pokemon)

# The aggregate function
# We can use the aggregate command to aggregate a numeric feature by a categorical one.

# The aggregate function has three parameters
# 1. The numeric value, e.g. sales, to be aggregated to find out, e.g., total of sales,
#   average of sales, number of sales (i.e. orders).
# 2. The set of categories, product_category and sales_region, on which you wish
#   to aggregate
# 3.The aggregation function (e.g., sum, mean, length) that you wish to use
aggregate(Attack~Legendary, summary, data = pokemon)
aggregate(Type.1~Legendary, summary, data = pokemon)
aggregate(Attack~Type.1, summary, data = pokemon)
aggregate(HP~Legendary, mean, data = pokemon)










# Part 2: Pokemon Battle Prediction
### ---------------------------------------------------------------------------

### 1. Predicting with only combat history

# Import combats information from csv
combats <- read.csv(file = "combats.csv", stringsAsFactors = FALSE)

# str() shows the structure of data
str(combats)

# summary() shows the summary of data
summary(combats)

# Change categorical variables to factors
combats$First_pokemon = as.factor(combats$First_pokemon)
combats$Second_pokemon = as.factor(combats$Second_pokemon)
str(combats)

# exam the unique number of pokemon
total_pokemon = c(combats[,0],combats[,1])
length(unique(total_pokemon))

# construct target variable
combats$Winner = ifelse(combats$Winner == combats$First_pokemon, 1, 0)
combats$Winner = as.factor(combats$Winner)
combats$Winner
summary(combats$Winner)

# Partition the dataset 
library(caret)
set.seed(1)
train_index <- createDataPartition(combats$Winner, p=0.7, list=FALSE)
datTrain <- combats[train_index,]
datTest <- combats[-train_index,]

# Build decision model 
library(rpart)
library(rminer)
library(rpart.plot)
rpart_model <- rpart(Winner~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 5))
rpart.plot(rpart_model)
rpart_model

# Make predictions on both training and tessting sets
prediction_on_train_rpart <- predict(rpart_model, datTrain)
prediction_on_test_rpart <- predict(rpart_model, datTest)

# Compare the evaluation results on training and testing sets 
mmetric(datTrain$Winner,prediction_on_train_rpart, metric="CONF")
mmetric(datTest$Winner,prediction_on_test_rpart, metric="CONF")
mmetric(datTrain$Winner,prediction_on_train_rpart,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Winner,prediction_on_test_rpart,metric=c("ACC","PRECISION","TPR","F1"))

# knn model (won't work)
#knn_model <- knn3(Winner~.,data = datTrain, k=5)

# Make predictions on both training and tessting sets
#prediction_on_train_rpart2 <- predict(knn_model, datTrain)
#prediction_on_test_rpart2 <- predict(knn_model, datTest)

# Compare the evaluation results on training and testing sets 
#mmetric(datTrain$Winner,prediction_on_train_rpart2, metric="CONF")
#mmetric(datTest$Winner,prediction_on_test_rpart2, metric="CONF")
#mmetric(datTrain$Winner,prediction_on_train_rpart2,metric = c("ACC","PRECISION","TPR","F1"))
#mmetric(datTest$Winner,prediction_on_test_rpart2,metric=c("ACC","PRECISION","TPR","F1"))

# Naive Bayes
library(e1071)
library(rminer)
model_nb <- naiveBayes(Winner~.,data=datTrain, laplace = 1)
model_nb

# Make predictions on both training and tessting sets
prediction_on_train_rpart3 <- predict(model_nb, datTrain)
prediction_on_test_rpart3 <- predict(model_nb, datTest)

# Compare the evaluation results on training and testing sets 
mmetric(datTrain$Winner,prediction_on_train_rpart3, metric="CONF")
mmetric(datTest$Winner,prediction_on_test_rpart3, metric="CONF")
mmetric(datTrain$Winner,prediction_on_train_rpart3,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Winner,prediction_on_test_rpart3,metric=c("ACC","PRECISION","TPR","F1"))


### 2. Predicting with Pokemon Information
str(pokemon)
str(combats)
index1 = as.numeric(as.character(combats$First_pokemon))
index2 = as.numeric(as.character(combats$Second_pokemon))
pokemon1_infor = pokemon[index1,]
pokemon2_infor = pokemon[index2,]
pokemon_battle = cbind(first = pokemon1_infor,second=pokemon2_infor,Winner = combats$Winner)
str(pokemon_battle)

# check the winning rate of each type of pokemon
win = c(as.character(pokemon_battle[which(pokemon_battle$Winner==1),1]), as.character(pokemon_battle[which(pokemon_battle$Winner!=1),11]))
total_type = as.factor(c(as.character(pokemon_battle[,1]), as.character(pokemon_battle[,11])))
summary(total_type)
summary(as.factor(win))/summary(total_type)

# Partition the dataset 
set.seed(1)
train_index <- createDataPartition(pokemon_battle$Winner, p=0.7, list=FALSE)
datTrain <- pokemon_battle[train_index,]
datTest <- pokemon_battle[-train_index,]

# Build decision model 
rpart_model <- rpart(Winner~.,data = datTrain,control = rpart.control(cp = 0.0001, maxdepth = 12))
rpart.plot(rpart_model)
rpart_model

# Make predictions on both training and tessting sets
prediction_on_train_rpart <- predict(rpart_model, datTrain)
prediction_on_test_rpart <- predict(rpart_model, datTest)

# Compare the evaluation results on training and testing sets 
mmetric(datTrain$Winner,prediction_on_train_rpart, metric="CONF")
mmetric(datTest$Winner,prediction_on_test_rpart, metric="CONF")
mmetric(datTrain$Winner,prediction_on_train_rpart,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Winner,prediction_on_test_rpart,metric=c("ACC","PRECISION","TPR","F1"))

# knn model (won't work)
#knn_model <- knn3(Winner~.,data = datTrain, k=5)

# Make predictions on both training and tessting sets
#prediction_on_train_rpart2 <- predict(knn_model, datTrain)
#prediction_on_test_rpart2 <- predict(knn_model, datTest)

# Compare the evaluation results on training and testing sets 
#mmetric(datTrain$Winner,prediction_on_train_rpart2, metric="CONF")
#mmetric(datTest$Winner,prediction_on_test_rpart2, metric="CONF")
#mmetric(datTrain$Winner,prediction_on_train_rpart2,metric = c("ACC","PRECISION","TPR","F1"))
#mmetric(datTest$Winner,prediction_on_test_rpart2,metric=c("ACC","PRECISION","TPR","F1"))

# Naive Bayes
model_nb <- naiveBayes(Winner~.,data=datTrain, laplace = 1)
model_nb

# Make predictions on both training and tessting sets
prediction_on_train_rpart3 <- predict(model_nb, datTrain)
prediction_on_test_rpart3 <- predict(model_nb, datTest)

# Compare the evaluation results on training and testing sets 
mmetric(datTrain$Winner,prediction_on_train_rpart3, metric="CONF")
mmetric(datTest$Winner,prediction_on_test_rpart3, metric="CONF")
mmetric(datTrain$Winner,prediction_on_train_rpart3,metric = c("ACC","PRECISION","TPR","F1"))
mmetric(datTest$Winner,prediction_on_test_rpart3,metric=c("ACC","PRECISION","TPR","F1"))



