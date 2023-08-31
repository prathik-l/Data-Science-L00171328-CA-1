> data <- read.csv("diabetes.csv", header = TRUE, sep = ",")
# Assuming you've loaded your data into a dataframe named 'df'

# Create a contingency table
contingency_table <- table(data$Diabetes_binary, data$HighBP)

# Perform the Chi-Square test
chi2_test <- chisq.test(contingency_table)

# If there are any cells in the contingency table with expected counts below 5,
# we might consider using Fisher's Exact Test
if (any(chi2_test$expected < 5)) {
  fisher_test <- fisher.test(contingency_table)
  print(fisher_test)
} else {
  print(chi2_test)
}



# View the first few rows of the dataset
head(data)

# Get a summary of the data structure
str(data)

# Summary of the two variables of interest
summary(data$Diabetes_binary)
summary(data$HighBP)

df$Diabetes_binary <- as.factor(data$Diabetes_binary)
df$HighBP <- as.factor(data$HighBP)

# Frequency table for diabetes and high blood pressure
table(data$Diabetes_binary)
table(data$HighBP)

# Crosstabulation
table(data$Diabetes_binary, data$HighBP)

cor(data$Diabetes_binary, data$HighBP)

# Using ggplot2 library for visualization
library(ggplot2)

# Bar plot for 'Diabetes_binary'
ggplot(data, aes(x=Diabetes_binary)) +
  geom_bar(aes(fill=Diabetes_binary), alpha=0.7) +
  labs(title="Distribution of Diabetes_binary", x="Diabetes Status", y="Count") +
  theme_minimal()

# Bar plot for 'HighBP'
ggplot(data, aes(x=HighBP)) +
  geom_bar(aes(fill=HighBP), alpha=0.7) +
  labs(title="Distribution of High Blood Pressure", x="HighBP Status", y="Count") +
  theme_minimal()

# QQ-plot for 'Diabetes_binary'
qqnorm(data$Diabetes_binary, main="QQ-Plot for Diabetes_binary")
qqline(data$Diabetes_binary, col="blue")

# QQ-plot for 'HighBP'
qqnorm(data$HighBP, main="QQ-Plot for High Blood Pressure")
qqline(data$HighBP, col="red")


