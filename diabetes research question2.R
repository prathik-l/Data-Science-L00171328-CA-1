# Assuming you have your data in a dataframe named 'dataframe_name'
# Replace 'dataframe_name' with the name of your dataframe
getwd()
data <- read.csv("diabetes.csv", header = TRUE, sep = ",")
# Split the data based on diabetes status
# Assuming you've loaded your data into a dataframe named 'df'

# Split the data based on diabetes status
group_diabetes <- data[data$Diabetes_binary == 1,]$BMI
group_no_diabetes <- data[data$Diabetes_binary == 0,]$BMI

# Histograms for both groups
hist(group_diabetes, main="Histogram for BMI of Individuals with Diabetes", 
     xlab="BMI", col="blue", border="black")
hist(group_no_diabetes, main="Histogram for BMI of Individuals without Diabetes", 
     xlab="BMI", col="red", border="black")

# QQ-plots for both groups
qqnorm(group_diabetes, main="QQ-Plot for BMI of Individuals with Diabetes")
qqline(group_diabetes, col="blue")
qqnorm(group_no_diabetes, main="QQ-Plot for BMI of Individuals without Diabetes")
qqline(group_no_diabetes, col="red")

# Levene's Test for Equality of Variances
install.packages("car")
library(car)
leveneTest(BMI ~ Diabetes_binary, data = data)

# Independent Samples t-test
t_test_result <- t.test(group_diabetes, group_no_diabetes)

# Wilcoxon Rank Sum test
wilcox_test_result <- wilcox.test(group_diabetes, group_no_diabetes)

# Print the results
print(t_test_result)
print(wilcox_test_result)

# Check the structure of the dataset
str(data)
# Summary of the two variables of interest
summary(data$Diabetes_binary)
summary(data$BMI)


# Boxplot to visualize the distribution of BMI based on diabetes status
boxplot(BMI ~ Diabetes_binary, data=data, main="BMI distribution by Diabetes Status",
        xlab="Diabetes Status", ylab="BMI")

# Calculate correlation between BMI and Diabetes_binary
cor(data$Diabetes_binary, data$BMI, method="spearman")

# Removing rows with NAs in our columns of interest
data_clean <- data[!is.na(data$Diabetes_binary) & !is.na(data$BMI), ]


