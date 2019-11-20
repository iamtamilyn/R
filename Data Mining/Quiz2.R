##Data description

'In order for a health insurance company to make money, it needs to collect more
in yearly premiums than it spends on medical care to its beneficiaries. As a result, 
insurers invest a great deal of time and money in developing models that accurately 
forecast medical expenses for the insured population.

Medical expenses are difficult to estimate because the most costly conditions are 
rare and seemingly random. Still, some conditions are more prevalent for certain 
segments of the population. For instance, lung cancer is more likely among smokers 
than non-smokers, and heart disease may be more likely among the obese.

The goal of this analysis is to use patient data to estimate the average medical
care expenses for such population segments. These estimates can be used to create 
actuarial tables that set the price of yearly premiums higher or lower, 
depending on the expected treatment costs.'

### ---------------------------------------------------------------------------
# The insurance data set has 1338 observations of 7 variables.
# We will use this file to predict the medical expenses.

# VARIABLE DESCRIPTIONS:
#age:	      age in years
#sex:	      gender
#bmi:	      body mass index
#children:	how many children do they have?
#smoker:	  do they smoke?
#region:	  geographic region
#expenses:	yearly medical expenses
### ---------------------------------------------------------------------------

# 1. Import the datadet
insurance <- read.csv(file = "insurance.csv", stringsAsFactors = FALSE)

# 2. str() shows the structure of data
str(insurance)

# 3. summary() shows the mean and the five-number statistics indicating the spread of each column's values
summary(insurance)

# 4. Change all categorical variables to factors
insurance$sex <- factor(insurance$sex)
insurance$smoker <- factor(insurance$smoker)
insurance$region <- factor(insurance$region)
str(insurance)
summary(insurance)

# 5. Partition the dataset: 70% for training, 30% for testing   
library(caret)
set.seed(1)
train_index <- createDataPartition(insurance$expenses, p=0.7, list=FALSE)
datTrain <- insurance[train_index,]
datTest <- insurance[-train_index,]

#6. Build a multiple linear regression model and display the training and testing performances (2 points)
library(rminer)



# 7.  Build a SVM model with C=5 and display the training and testing performances (2 points)
library(kernlab)




