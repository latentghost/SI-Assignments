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
# Define the significan level alpha = 5% = 0.05
mu_null <- 0
alpha <- 0.05

# Define test statistic as the t-stat
t_stat <- ((mu1 - mu2) - mu_null) / sqrt((std1^2) / n1 + (std2^2) / n2)

# Calculate the quantile corresponding to significance level alpha
# H1 corresponds to a rejection region on both sides of the critical values
quant <- qt(1 - alpha / 2, df = 8, lower.tail = TRUE)

cat("\nCritical Values:", -quant, "and", quant, "\n")

# H0 is rejected if the test statistic falls outside the region between the critical values
if (abs(t_stat) > quant) {
    cat("H\u2080 is rejected i.e. Population means are not likely to be equal.\n")
} else {
    cat("H\u2080 is accepted i.e. Population means are likely to be equal.\n")
}
cat("\n")

# Calculate the p-value corresponding to the test statistic
# Since H1 corresponds to both sides of H0, the rejection region
# lies on both sides of the curve
p_value <- 2 * pt(q = -abs(t_stat), df = 8, lower.tail = TRUE)

cat("p-value:", p_value, "\n")

if (p_value < alpha) {
    cat("H\u2080 is rejected i.e. Population means are not likely to be equal.\n")
} else {
    cat("H\u2080 is accepted i.e. Population means are likely to be equal.\n")
}
cat("\n")