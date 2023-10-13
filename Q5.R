library(stats)

rm(list = ls())

# Define the two sample data sets and diff as the difference between the two samples
s1 <- c(49, 53, 51, 52, 47, 50, 52, 53)
s2 <- c(52, 55, 52, 53, 50, 54, 54, 53)
diff <- s2 - s1

# Define significance level alpha  = 5% = 0.05
alpha <- 0.05

# Since data is available, paired t-test is used to determine if the mean difference is significantly different from 0
# Define H0; mu1 - mu2 = 0 versus H1: abs(mu1 - mu2) != 0
test <- t.test(diff, mu = 0, alternative = "two.sided", conf.level = alpha)

cat("\nt-stat:", test$statistic)
cat("\nNull value:", test$null.value)
cat("\nEstimated mean:", test$estimate)
cat("\nConfidence interval:", test$conf.int[1], "to", test$conf.int[2])

p_value <- test$p.value

cat("\np-value:", p_value,"\n")
cat("\n")

if (p_value > alpha) {
    cat("H\u2080 is accepted i.e. the average change in weight of children due to food B is likely to be 0.","\n")
} else {
    cat("H\u2080 is rejected i.e. there is a significant average change in weight of children due to food B.","\n")
}
cat("\n")
