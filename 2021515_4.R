library(stats)

rm(list = ls())

# Define the sample data with index 1 corresponding to girls and 2 to boys
n1 <- 9
n2 <- 16
mu1 <- 2.0
mu2 <- 3.2
std1 <- sqrt(0.75)
std2 <- 1.0

# Define H0: mu1 = mu2 and H1: mu1 != mu2
# Define null value as 0 (since H0 is same as mu1 - mu2 = 0)
mu_null <- 0

#Define test statistic as the t-stat
t_stat <- ((mu1 - mu2) - mu.null) / sqrt(((std1^2) / n1) + ((std2^2) / n2))



# Calculate the p-value corresponding to the test statistic
# Since H1 corresponds to both sides of H0, the rejection region
# lies on both sides of the curve
p_value <- 2 * pt()