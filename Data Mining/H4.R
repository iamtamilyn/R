##### IS 4482 Homework 4 ----------------------------------------------------------

#### Clustering and Association Rule Mining ---------------------------------------


# Part 1. Clustering with Walmart Visit data

### -------------------------------------------------------------------------------
#Data: Walmart held its 3rd Kaggle “recruiting” competition (https://www.kaggle.com/c/walmart-recruiting-trip-type-classification) 
#in Fall 2015 to attract data scientists interested in getting jobs at Walmart. 

#Walmart uses both art and science to continually make progress on their core mission 
#of better understanding and serving their customers. One way Walmart is able to improve 
#customers' shopping experiences is by segmenting their store visits into different trip types. 

#Whether they're on a last minute run for new puppy supplies or leisurely making their way 
#through a weekly grocery list, identify trip types enables Walmart to create the best 
#shopping experience for every customer.


# VARIABLE DESCRIPTIONS:
# unique_items – the number of unique UPC numbers of the products purchased in a visit
# total_purchase_quantity - the total number of the items that were purchased in a visit 
# total_return_quantity -  the total number of the items returned in a visit
# net_quantity = total_purchase_quantity – total_return_quantity
# unique_departments – the number of unique departments representing the purchased items in a visit.
# departments_with_returns – the number of unique departments representing the returned items in a visit.
### -------------------------------------------------------------------------------


# 1. Import dataset
Walmart <- read.csv(file = "Walmart_2015_visits_sample.csv", stringsAsFactors = TRUE)
str(Walmart)
summary(Walmart)

# 2. Perform k-mean clustering with number of cluster k = 2 and display the clustering results (3 points)
library(RWeka)


# What are the size and centroids of each cluster? (2 points)



# 3. Perform k-mean clustering with number of cluster k = 3 and display the clustering results (3 points)



# What are the size and centroids of each cluster? (3 points)



# Interpret each cluster based on your understanding. (3 points)



# Assign a meaningful name to each cluster based on the purchasing pattern. Explain the reasons for the name you choose. (3 points)
# To give a few hypothetical examples of trip types: a customer may make a small daily dinner trip, a weekly large grocery trip, a trip to buy gifts for an upcoming holiday, or a trip to buy seasonal items.



# What marketing strategy you can use based on your clustering result? (3 points)



# 4. Perform k-mean clustering with number of cluster k = 5 and display the clustering results (3 points)



# Based on the cluster size and centroids, which k (k=2, k=3, or k=5) provides better clustering results, and why? (2 points)


# 5. elbow test/scree plot to determine optimal number of clusters.
# Step 1: initial a vector contains a list of k values from 1 to 10.
a_list_of_k = c(1:10)
# Step 2: initial a vector that contains errors
errors = vector()
# Step 3: create a for loop: for each k in the list of k (from 1 to 10)
# We run a simpleKMeans model based on that k and store the value in errors vector.
for (k in a_list_of_k) {
  model <- SimpleKMeans(Walmart, Weka_control(N=k))
  errors[k] = model$clusterer$getSquaredError()
  print(errors[k])
}
#Step 4: Plot the elbow plot: x axis = k and y axis = errors
plot(x = a_list_of_k, y = errors, xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares Error", type="b")


# What is the optimal k? (2 points)





# Part 2. Association Rule Mining with Walmart basket data

### -------------------------------------------------------------------------------
#Walmart_2015_dept_baskets.csv is derived from the same data source to represent shopping 
#baskets in the long format based on VisitNumber as transaction id and DepartmentDescription 
#as a high level indication of item type in a basket
### -------------------------------------------------------------------------------
# 1. Import Walmart_2015_dept_baskets.csv file
library(arules)
Dept_baskets <- read.transactions("Walmart_2015_dept_baskets.csv", format="single", sep = ",", header = TRUE, cols=c("VisitNumber","DepartmentDescription"))
summary(Dept_baskets)

# How many transactions and items are in this data? (1 points)



# 2. Inspect the departments in the first 5 transactions (2 points)



# 3. Examine the relative frequency of items in descending order (2 points)



# What are the top 3 most frequent items? (1.5 points)



# What are the support values for top 3 most frequent items? (1.5 points)



# 4. Examine the absolute frquency of items in descending order (2 points)



# 5. Use the apriori command to  generate rules with minimal support = 0.05 and minimal confidence = 0.25 and minlen = 3 (3 points)



# Display the rules in the descending order of their lift values (2 points)



# How many rules are generated? (1 points)



# Interpret the first rule based on the values of the support, confidence, and lift (3 points)



# Find and display rules containing "DAIRY" (1 points)



# Find and display rules containing "DAIRY" on the left-hand side (1 points)




# Find and display rules containing "DAIRY" on the right-hand side (1 points)




# Does DAIRY have positive effect on itemset {COMM BREAD,GROCERY DRY GOODS}? (1 points)




