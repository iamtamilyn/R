# 1. Assignment Operator
x <- 2
print(x)
x = 1
print(x)
msg = "hello"
msg

# 2. Use the colon operator to create integer sequences
x = 1:20
x

# 3. Create a character vector
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

# 4. Obtain body temp for Jane Doe
temperature[2]

# 5. Range of values using colon
temperature[2:3]

# 6. exclude jane doe's temp data
temperature[-2]

# 7. create a factor from a character vector
gender <- factor(c("MALE", "FEMALE", "MALE"))
gender

# 8. create factors with additional levels
blood <- factor(c("O", "AB", "A"), levels = c("A", "B", "AB", "O"))
blood[1:2]

# 9. create a list 
subject1 <- list(fullname = subject_name[1],
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1])
subject1

# 10. Access the temperature value in created list
subject1[2]

subject1$temperature

# 11. create a data frame for our patient dataset
pt_data <- data.frame(subject_name, temperature, flu_status, gender, blood, stringsAsFactors = FALSE)
pt_data

# 12. Obtain the subject_name vector from the data frame
pt_data$subject_name

# 13. Extract the first and second columns from the data frame
pt_data[c("subject_name","temperature")]
pt_data[c(1,2)]

# 14. Extract the value in the first row and second column
pt_data[1,2]

# 15. Extract the first columns from data frame
pt_data[,1]

# 16. Extract the first row from data frame
pt_data[1,]

# 17. Extract everything from data frame
pt_data[,]

# 18. Exclude the first column from the data frame
pt_data[, -1]

# 19. Exclude the first row from data frame
pt_data[-1,]