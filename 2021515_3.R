library(stats)

rm(list = ls())

# Define the sample data
data <- c(14.3, 12.6, 13.7, 10.9, 13.7, 12.0, 11.4, 12.0, 12.6, 13.1)

# Calculate the mean and standard deviation for the sample data
n <- length(data)
xbar <- mean(data)
std <- sd(data)

# Define H0: mu = 12 versus H1: mu != 12
# Define null value = 12
# Define significance level alpha = 5% = 0.05
mu_null <- 12.0
alpha <- 0.05

# Define test statistic as the t-stat (since popn variance is unknown)
t_stat <- (xbar - mu_null) / (std / sqrt(n))

# Calculate the quantile corresponding to significane level alpha
quant <- -c(qt(1 - (alpha / 2), n - 1), qt(alpha / 2, n - 1))

cat("\nCritical Values:", quant, "\n")

# H0 is rejected if the test statistic falls outside the region between the critical values
if (t_stat < quant[1] || t_stat > quant[2]) {
    cat("H\u2080 is rejected i.e. the yield is not likely to be 12.0 quintals per hectare.", "\n")
} else {
    cat("H\u2080 is accepted i.e. the yield is likely to be 12.0 quintals per hectare.", "\n")
}
cat("\n")

# Calculate the p-value corresponding to the test statistic
# Since H1 corresponds to both sides of H0, the rejection region lies on both sides of the curve
p_value <- pt(q = t_stat, df = n - 1, lower.tail = TRUE)

cat("p-value:", p_value, "\n")

if (p_value > alpha) {
    cat("H\u2080 is accepted i.e. the yield is likely to be 12.0 quintals per hectare.", "\n")
} else {
    cat("H\u2080 is rejected i.e. the yield is not likely to be 12.0 quintals per hectare.", "\n")
}
cat("\n")