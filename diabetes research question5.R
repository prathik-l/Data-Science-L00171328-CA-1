> data <- read.csv("diabetes.csv", header = TRUE, sep = ",")

# Check for missing values and remove them
data_clean <- data[!is.na(data$Education) & !is.na(data$Diabetes_binary), ]
# Create a contingency table
contingency_table <- table(data_clean$Education, data_clean$Diabetes_binary)
print(contingency_table)

# Perform the chi-square test
chi_test <- chisq.test(contingency_table)
print(chi_test)

# Calculate Cramér's V
cramersV <- sqrt(chi_test$statistic / (nrow(data_clean) * (min(dim(contingency_table)) - 1)))
print(cramersV)
# Check the structure of the dataset
str(data)
# Summary of the two variables of interest
summary(data$Diabetes_binary)
summary(data$Education)
# Frequency tables for both variables
table(data$Diabetes_binary)
table(data$Education)

# Bar plots for visual representation of the distributions
barplot(table(data$Diabetes_binary), main="Distribution of Diabetes_binary", 
        col=c("blue", "red"))
barplot(table(data$Education), main="Distribution of Education", 
        col=rainbow(length(unique(data$Education)))) # Adjust colors based on the number of categories
# Chi-square test to get the statistic for Cramér's V
chisq <- chisq.test(table(df$Diabetes_binary, df$Education))

# Calculate Cramér's V
cramersV <- sqrt(chisq$statistic / (sum(table(data$Diabetes_binary, data$Education)) * (min(dim(table(data$Diabetes_binary, data$Education))) - 1)))
print(cramersV)
