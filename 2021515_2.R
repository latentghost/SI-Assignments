rm(list = ls())

# Define H0: mu = 15 and H1: mu != 15
# Define xbar = 17.4, sigma = 6.3, null value = 15 (given)
# Define significance level alpha = 5% = 0.05

n <- 75
xbar <- 17.4
std <- 6.3
mu_null <- 15
alpha <- 0.05

# Define test statistic as the t-stat
t_stat <- (xbar - mu_null) / (std / sqrt(n))

# Calculate the p-value corresponding to the test statistic
# Since H1 corresponds to both sides of H0, the rejection region
# lies on both sides of the curve
p_value <- pt(q = t_stat, df = n - 1, lower.tail = TRUE)

cat("\np-value:", p_value, "\n")

if (p_value > alpha) {
    cat("H\u2080 is accepted i.e. the population mean time on death row could likely be 15 years.", "\n")
} else {
    cat("H\u2080 is rejected i.e. the population mean time on death row could is not 15 years.", "\n")
}
cat("\n")