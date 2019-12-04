### -------------------------------------------------------------------------------
#Market basket analysis is used behind the scenes for the recommendation systems 
#used in many brick-and-mortar and online retailers. The learned association rules 
#indicate the combinations of items that are often purchased together. Knowledge 
#of these patterns provides insight into new ways a grocery chain might optimize 
#the inventory, advertise promotions,or organize the physical layout of the store. 
#For instance, if shoppers frequently purchase coffee or orange juice with a breakfast 
#pastry, it may be possible to increase profit by relocating pastries closer to coffee 
#and juice.

#In this lab, we will perform a market basket analysis of transactional data 
#from a grocery store.Our market basket analysis will utilize the purchase data 
#collected from one month of operation at a real-world grocery store. The data contains 
#9,835 transactions.
### ------------------------------------------------------------------------------

# 1. Import groceries.csv file
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")

# 2. Understanding of your data.
# Summary of dataset
summary(groceries)

# Inspect the first 5 transactions
inspect(groceries[1:5])

# How many transactions and items in this data?
# 9835 transactions and 169 items in total

# 3. Data exploration
# Examine the relative frequency of items in descending order
sort(itemFrequency(groceries, type="relative"), decreasing = TRUE)

# What are the top 3 most frequent items?
# whole milk, other vegetables, rolls/buns

# What are the support values for top 3 most frequent items?
# whole milk: 0.2555160142, other vegetables: 0.1934926284, rolls/buns: 0.1839349263

# Examine the absolute frquency of items in descending order
sort(itemFrequency(groceries, type="absolute"), decreasing = TRUE)

# Plot the most frequent 8 items in the descending order of transaction frequency in percentage
itemFrequencyPlot(groceries, type="relative", topN = 8)

# 4. Use the apriori command to generate rules with minimal support = 0.01 and minimal confidence = 0.3 and max length = 2.
groceries_rules <- apriori(groceries, parameter = list(support = 0.01, confidence = 0.3, maxlen=2))
summary(groceries_rules)

# Display all rules sorted by confidence levels.
inspect(sort(groceries_rules, by = "confidence"))

# Display top 5 rules
inspect(sort(groceries_rules, by = "confidence")[1:5])

# Display all rules sorted by confidence levels.
inspect(sort(groceries_rules, by = "confidence"))

# What is the probability of buying whole milk when butter is purchased?
# If customers bought butter, they will have 49.72% chance of buying whole milk.

# What is the probability of buying butter and whole milk together?
# The probability of buying butter and whole milk together is 0.02755465.

# Interpret the first rule based on the values of the support, confidence, and lift.
# The probability of buying butter and whole milk together is 0.02755465. There are 2.755465% transactions indluding both butter and whole milk.
# When customers bought butter, they will also buy whole milk about 49.72477% of the time. Among transactions including butter, 49.72477% of them also include whole milk.
# The probability of buying whole milk is increased by 1.946053 times if the customer bought butter. The probability of buying butter is increased by 1.946053 times if the customer bought whole milk.
# Butter and whole milk have positive effect (because lift value greater than 1) on each other.


# 5. Use the apriori command to  generate rules with minimal support = 0.02 and minimal confidence = 0.4 and max length = 3.
groceries_rules2 <- apriori(groceries, parameter = list(support = 0.02, confidence = 0.4, maxlen=3))

# Display top 10 rules for Task 2 sorted by lift.
inspect(sort(groceries_rules2, by = "lift")[1:10])

# Find and display rules containing "other vegetables"
vegetables_rules <- subset(groceries_rules2, items %in% "other vegetables")
inspect(vegetables_rules)

# Find and display rules containing "other vegetables" on the left-hand side
vegetables_rules_l <- subset(groceries_rules2, lhs %in% "other vegetables")
inspect(vegetables_rules_l)

# Interpret the first rule (containing "other vegetables" on the left-hand side) based on support, confidence, and lift values.
# The probability of buying other vegetables,root vegetables and whole milk together is 0.02318251.
# When customers bought other vegetables and root vegetables, they will also buy whole milk about 48.92704% of the time.
# The probability of buying whole milk is increased by 1.914833 times if the customer bought other vegetables and root vegetables The probability of buying other vegetables and root vegetables is increased by 1.914833 times if the customer bought whole milk.

# Find and display rules containing "other vegetables" on the right-hand side 
vegetables_rules_r <- subset(groceries_rules2, rhs %in% "other vegetables")
inspect(vegetables_rules_r)


# 6. Use the apriori command to generate about 30 to 50 association rules. Set your own minimum support and confidence threshold levels. 
# Remember if the thresholds are too low, you will get too many rules, or if you set them too high, you may not get any or enough rules.
groceries_rules3 <- apriori(groceries, parameter = list(support = 0.02, confidence = 0.3))

# Inspect all of the rules in the descending lift values of the rules.
inspect(sort(groceries_rules3, by = "lift"))

# Select an interesting rule and explain how it can benefit the grocery store.



