
data <- read.csv("diabetes.csv", header = TRUE, sep = ",")


# Assuming you've loaded your data into a dataframe named 'df'

# Create a contingency table
contingency_table <- table(data$Diabetes_binary, data$PhysActivity)

# View the table
print(contingency_table)

# Check for expected frequencies
chisq <- chisq.test(contingency_table)
print(chisq$expected)

# If all expected frequencies are greater than 5, perform the Chi-Square test
if (all(chisq$expected >= 5)) {
  print(chisq)
} else {
  cat("Some expected frequencies are below 5. Consider a different test or grouping categories.")
}
# Check the structure of the dataset
str(data)
# Summary of the two variables of interest
summary(data$Diabetes_binary)
summary(data$PhysActivity)

# Frequency tables for both variables
table(data$Diabetes_binary)
table(data$PhysActivity)

# Bar plots for visual representation
barplot(table(data$Diabetes_binary), main="Distribution of Diabetes_binary", 
        col=c("blue", "red"))
barplot(table(data$PhysActivity), main="Distribution of Physical Activity", 
        col=c("green", "orange", "purple")) # Adjust colors based on the number of categories

# Chi-square test for independence
chisq <- chisq.test(table(data$Diabetes_binary, data$PhysActivity))

# Calculate Cramér's V
cramersV <- sqrt(chisq$statistic / (sum(table(data$Diabetes_binary, data$PhysActivity)) * (min(dim(table(data$Diabetes_binary, data$PhysActivity))) - 1)))
print(cramersV)

# Removing rows with NAs in our columns of interest
data_clean <- data[!is.na(data$Diabetes_binary) & !is.na(data$PhysActivity), ]

