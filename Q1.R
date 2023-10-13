library(stats)

rm(list = ls())

# Define H0: mu <= 15 versus H1: mu > 15
# Define xbar = 17, sigma = 0.05, alpha = 5%, null value = 15

n <- 10
xbar <- 17
std <- 0.5
mu_null <- 15
alpha <- 0.05

# Define test statistic as the z-stat (since popn variance is known)
z_stat <- (xbar - mu_null) / (std / sqrt(n))

# Calculate the quantile corresponding to significane level alpha
quant <- qnorm(1 - alpha, mean = 0, sd = 1, lower.tail = FALSE)

cat("\nCritical Value:", quant, "\n")

# H0 is rejected if the test statistic goes beyond the critical value
if (z_stat <= quant) {
    cat("H\u2080 is accepted i.e. the bread height is likely to be less than 15 cm.", "\n")
} else {
    cat("H\u2080 is rejected i.e. the bread height is likely to be more than 15 cm.", "\n")
}
cat("\n")

# Calculate the p-value corresponding to the test statistic
# Since H1 corresponds to the right side, upper tail value is used
p_value <- pnorm(z_stat, mean = 0, sd = 1, lower.tail = FALSE)

cat("p-value:", p_value, "\n")

if (p_value > alpha) {
    cat("H\u2080 is accepted i.e. the bread height is likely to be less than 15 cm.", "\n")
} else {
    cat("H\u2080 is rejected i.e. the bread height is likely to be more than 15 cm.", "\n")
}
cat("\n")
