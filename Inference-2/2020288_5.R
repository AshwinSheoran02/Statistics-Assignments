rm(list=ls())
# H0 : mu of difference of pairs = 0 vs H1 : mu of difference of pairs != 0
# Data
food_A <- c(49, 53, 51, 52, 47, 50, 52, 53)
food_B <- c(52, 55, 52, 53, 50, 54, 54, 53)
n <- 8

# Calculate the difference in weight for each child
diff_weight <- food_B - food_A

# Calculate the sample mean and standard deviation of the difference
sample_mean <- mean(diff_weight)
sample_sd <- sd(diff_weight)

# Set the significance level
alpha <- 0.05

# Calculate the test statistic and p-value
test_stat <- (sample_mean - 0) / (sample_sd / sqrt(length(diff_weight)))
p_value <- 2 * pt(abs(test_stat), df = length(diff_weight) - 1, lower.tail = FALSE)
left_critical_value <- qt(p = alpha/2,df=n-1,lower.tail = TRUE)
right_critical_value <- qt(p = alpha/2,df=n-1,lower.tail = FALSE)

# Print the results
cat ("Sample Mean: ",sample_mean,"\n" )
cat ("Sample Sd: ",sample_sd,"\n" )
cat("Test Statistics: ", test_stat, "\n")
cat("Critical Values: Left: ", left_critical_value, ", Right: ", right_critical_value, "\n")
cat("P-Value: ", p_value, "\n")

# Test the hypothesis
if ((test_stat < left_critical_value) | (test_stat > right_critical_value) | (p_value < alpha)) {
  cat("Reject the null hypothesis. There is a significant difference in the average change in weight of children due to Food B.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference in the average change in weight of children due to Food B.\n")
}
