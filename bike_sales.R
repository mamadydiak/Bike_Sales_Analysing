
library(readr)
library(ggplot2)
library(tidyverse)
library(skimr)
library(DT)
library(car)
library(ggpubr)
library(dplyr)

Data <- read.csv("Bike_Sales1.csv")
data <- data.frame(Data)

# Exploratory and descriptive data analysis:

datatable(data, options = list(scrollX = TRUE, scrollY = "200px"))

# Check for NA values in the entire dataset
is_na_data <- is.na(data)

# Summarize the NA values for each column
na_summary <- colSums(is_na_data)

vars_to_factor <- c("Date", "Month", "Age_Group","Customer_Gender","Country","State","Product_Category","Sub_Category","Product")

# Convert variables to factors using lapply
data[vars_to_factor] <- lapply(data[vars_to_factor], factor)
skim(data)

# Data Visualization:
# Histogram
ggplot(data, aes(x = Customer_Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Customer_Age Distribution", x = "Customer_Age", y = "Frequency")

# Box Plot for Profit by Age Group
ggplot(data, aes(x = Age_Group, y = Profit, fill = Age_Group)) +
  geom_boxplot() +
  labs(title = "Profit by Age Group", x = "Age Group", y = "Profit") +
  theme_minimal()

# Scatter Plot for Order Quantity vs. Profit
ggplot(data, aes(x = Order_Quantity, y = Profit)) +
  geom_point(color = "black") +
  labs(title = "Order Quantity vs. Profit", x = "Order Quantity", y = "Profit") +
  theme_minimal()

#Bar Chart for Age Group Distribution
ggplot(data, aes(x = Age_Group)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Age Group Distribution", x = "Age Group", y = "Count") +
  theme_minimal()

# Pie Chart for Customer Gender Distribution
gender_count <- data %>%
  count(Customer_Gender)

ggplot(gender_count, aes(x = "", y = n, fill = Customer_Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Customer Gender Distribution", x = "", y = "") +
  theme_minimal()

# Confidence Intervals:

calculate_confidence_itervals_and_normality <- function(data, variable) {
  
  # Calculate the mean for the variable
  mean_variable <- mean(data[[variable]])
  
  # Calculate the standard error for the variable
  se_variable <- sd(data[[variable]]) / sqrt(length(data[[variable]]))
  
  # Calculate the confidence intervals for the variable
  ci_variable <- mean_variable + c(-1.96, 1.96) * se_variable
  
  # Perform normality test
  normality_test <- shapiro.test(data[[variable]])
  
  # Check if confidence intervals are symmetric
  is_symmetric <- all(diff(ci_variable) > 0)
  
  if(!is_symmetric){
    
  }
  # Print the results
  cat("Variable:", variable, "\n")
  cat("Normality Test p-value:", normality_test$p.value, "\n")
  cat("Confidence Intervals:", ci_variable, "\n")
  cat("Is Symmetric:", is_symmetric, "\n\n")
  
}

calculate_confidence_itervals_and_normality(data, "Order_Quantity")

calculate_confidence_itervals_and_normality(data, "Profit")

#Transformation:
# Implement one transformation (log transformation, Box-Cok transformation, etc) for one of your quantitative variables, which is not normally distributed; but will be normal or more normal, after the transformation. Comment about your trial (3-4 sentences)

# Apply log transformation
data$Revenue <- log(data$Revenue)

calculate_confidence_itervals_and_normality(data,"Revenue")

# Checking the normality of 'Order_Quantity' variable
qqnorm(data$Order_Quantity, ylab = "Order_Quantity", xlab = "Theoretical Quantiles")
qqline(data$Order_Quantity)

# Checking the normality of 'Profit' variable
qqnorm(data$Profit, ylab = "Profit", xlab = "Theoretical Quantiles")
qqline(data$Profit)

#Assumption Homogeneity of Variances (using Fligner-Killeen test)

fligner.test(data$Order_Quantity, data$Profit)

# Perform independent samples t-test
t_test_result <- t.test(data$Order_Quantity, data$Profit)

# Print the result
print(t_test_result)

#7.ANOVA and Tukey Test

# Shapiro-Wilk test for normality
shapiro.test(data$Profit)

# Levene's test for homogeneity of variances
leveneTest(data$Profit, data$Order_Quantity, center = median)
selected_data <- data

#Categorize "Revenue" variable
selected_data$Revenue <- cut(selected_data$Revenue,
                             breaks = c(0, 150, 300, Inf),
                             labels = c("0-150", "150-300", "300+"))

#Categorize "Order_Quantity" variable
selected_data$Order_Quantity <- cut(selected_data$Order_Quantity,
                                    breaks = c(1, 25, 64, Inf),
                                    labels = c("1-25", "25-64", "64+"))

# Categorize "Profit" variable
selected_data$Profit <- cut(selected_data$Profit,
                            breaks = c(0, 200, 400, Inf),
                            labels = c("0-200", "200-400", "400+"))

# Perform ANOVA
result <- aov(as.numeric(Revenue) ~ Order_Quantity + Profit, data = selected_data)
print(result)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(result)

#  Print the Tukey result
print(tukey_result)

#Stepwise Regression Find the Best Model:
best_model <- step(lm(Order_Quantity ~Profit, data), direction = "both")

# Scatter plot
plot(data$Profit, data$Order_Quantity, xlab = "Profit", ylab = "Number of Order_Quantity ")

residuals <- resid(best_model)

hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

#normality plots
qqnorm(residuals)
qqline(residuals)

residuals <- resid(best_model)

plot(fitted(best_model), residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")

# Give the output of the best model 
summary(best_model)




