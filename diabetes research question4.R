# Assuming you've loaded your data into a dataframe named 'data'
> data <- read.csv("diabetes.csv", header = TRUE, sep = ",")


# Check for missing values and remove them
data_clean <- data[!is.na(data$Smoker) & !is.na(data$MentHlth), ]

# Histograms for visual normality check
hist(data_clean$MentHlth[data_clean$Smoker == 0], main="MentHlth for Non-smokers", xlab="Number of days of poor mental health", col="lightblue")
hist(data_clean$MentHlth[data_clean$Smoker == 1], main="MentHlth for Smokers", xlab="Number of days of poor mental health", col="pink")

# QQ-plots for both groups
qqnorm(data_clean$MentHlth[data_clean$Smoker == 0])
qqline(data_clean$MentHlth[data_clean$Smoker == 0])

qqnorm(data_clean$MentHlth[data_clean$Smoker == 1])
qqline(data_clean$MentHlth[data_clean$Smoker == 1])

# Levene's test for equality of variances
library(car)
leveneTest(MentHlth ~ Smoker, data = data_clean)

# If assumptions are met, perform the t-test
t_test <- t.test(MentHlth ~ Smoker, data = data_clean, var.equal = TRUE) # Set 'var.equal' based on Levene's test result
print(t_test)

# Check the structure of the dataset
str(data)
# Summary of the two variables of interest
summary(data$Smoker)
summary(data$MentHlth)
# Frequency tables for both variables
table(data$Smoker)
hist(data$MentHlth, main="Distribution of MentHlth", xlab="Number of days of poor mental health", col="lightblue")
# Point-biserial correlation
cor(data$Smoker, data$MentHlth, method="pearson")
# Removing rows with NAs in our columns of interest
data_clean <- data[!is.na(data$Smoker) & !is.na(data$MentHlth), ]

# Additionally, one can use boxplots or other techniques to detect and handle outliers for 'MentHlth'
boxplot(data_clean$MentHlth, main="Boxplot of MentHlth")

