rm(list=ls())

## Sample size

n<-10

## Data
x <- c(14.3, 12.6, 13.7, 10.9, 13.7, 12.0, 11.4, 12.0, 12.6, 13.1)
alpha <- 0.05

sample_mean<-mean(x)
sample_standard_deviation <- sd(x)
mu<-12
test_stat <- (sample_mean - mu)/(sample_standard_deviation/sqrt(n))

# Calculate the degrees of freedom
degree <- n - 1

p_value <- 2 * pt(-abs(test_stat), df = degree)


# Calculate the critical values from t-distribution
left_critical_value <- qt(alpha/2, df = degree, lower.tail = TRUE)
right_critical_value <- qt(alpha/2, df = degree, lower.tail = FALSE)

# Print the results
cat("Test statistic:", test_stat, "\n")
cat("P value:", p_value, "\n")

cat("Left critical value:", left_critical_value, "\n")
cat("Right critical value:", right_critical_value, "\n")

# Compare the test statistic with critical values and p-value with alpha
if ((test_stat < left_critical_value | test_stat > right_critical_value) | (p_value < alpha)) {
  cat("Reject the null hypothesis that the mean yield is 12.0 quintals per hectare.\n")
} else {
  cat("Fail to reject the null hypothesis that the mean yield is 12.0 quintals per hectare.\n")
}
