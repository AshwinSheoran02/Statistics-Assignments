rm(list = ls())

## Here H0 => mean time boys  spend  = mean time girls  spend on playing sports 
## vs
## H1 => mean time boys  spend != mean time girls  spend on playing sports

n_boys <- 16
n_girls <- 9
sample_mean_boys <- 3.2
sample_mean_girls <- 2
sample_var_boys <- 1
sample_var_girls <- 0.75
alpha <- 0.05
df <- min(n_boys -1 ,  n_girls - 1)

# Calculate the test statistic and critical values
test_stat <- (sample_mean_boys - sample_mean_girls) / sqrt((sample_var_boys/n_boys) + (sample_var_girls/n_girls))
left_critical_value <- qt(p = alpha/2, df = df, lower.tail = TRUE)
right_critical_value <- qt(p = alpha/2, df = df, lower.tail = FALSE)

# Calculate the p-value
p_value <- 2 * pt(q = abs(test_stat), df = df, lower.tail = FALSE)

# Print the results
cat("Test Statistics: ", test_stat, "\n")
cat("Critical Values: Left: ", left_critical_value, ", Right: ", right_critical_value, "\n")
cat("P-Value: ", p_value, "\n")

# Test the hypothesis
if ((test_stat < left_critical_value | test_stat > right_critical_value) | (p_value < alpha)) {
  cat("Reject the null hypothesis. The mean amount of time spent playing sports per day is different between boys and girls.\n")
} else {
  cat("Fail to reject the null hypothesis. \n")
}
