rm(list=ls())

set.seed(123) # set seed for reproducibility

## sample size
n<-10

popn_mean <- 17

popn_standard_deviation <- 0.5

mu <- 15

alpha <- 0.05

ztest_statistic<-(popn_mean - mu)/(popn_standard_deviation/sqrt(n))
cat("Test statistic: ",ztest_statistic,"\n")

# we calculate the critical value
# given alpha = 0.05, so for two-tailed we divide by 2
ztest_critical_value<-qnorm(p=alpha,0,1,lower.tail = FALSE)
cat("Test Critical Value: ",ztest_critical_value,"\n")
# Here the test_critical_value lies in the rejection region thus, we reject H0

p_value <-  pnorm(ztest_critical_value,0,1,lower.tail = FALSE)
cat("P value: ",p_value,"\n")

if(abs(ztest_statistic) > ztest_critical_value | p_value < alpha){
  cat("Reject the null hypothesis that bread height is <= 15 cm.\n")
} else {
  cat("Fail to reject the null hypothesis that bread height is <= 15 cm.\n")
}