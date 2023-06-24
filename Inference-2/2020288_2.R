rm(list=ls())
# Given values
n <- 75
sample_mean <- 17.4
sample_standard_deviation <- 6.3
mu <- 15
alpha <- 0.05

# Calculate the test statistic
test_stat <- (sample_mean - mu)/(sample_standard_deviation/sqrt(n))

# Calculate the degrees of freedom
df <- n - 1

# Calculate the critical values from t-distribution
left_critical_value <- qt(alpha/2, df = df, lower.tail = TRUE)
right_critical_value <- qt(alpha/2, df = df, lower.tail = FALSE)

# Calculate the p-value
p_value <- 2 * pt(abs(test_stat), df = df, lower.tail = FALSE)

# Print the results
cat("Test statistic:", test_stat, "\n")
cat("Left critical value:", left_critical_value, "\n")
cat("Right critical value:", right_critical_value, "\n")
cat("p-value:", p_value, "\n")

# Compare the test statistic with critical values and p-value with alpha
if(test_stat < left_critical_value | test_stat > right_critical_value | p_value < alpha){
  cat("Reject the null hypothesis that the population mean time on death row is 15 years.\n")
} else {
  cat("Fail to reject the null hypothesis that the population mean time on death row is 15 years.\n")
}
