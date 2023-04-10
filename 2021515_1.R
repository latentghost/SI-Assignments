rm(list=ls())

# random sample of size 1000 from Exponential (λ)
# ensures that the random sample generated is constant every time
set.seed(456)

# sample size
n <- 1000

# define lambda = 1,2,3,4
lambda_vals <- c(1, 2, 3, 4)

# define a function that returns the -ve log-likelihood
# of a given input data and given values of lambda
likelihood <- function(d,l){
    return (-(n*log(l) - l*(sum(d))))
}

# # create a plotting matrix of 2x2 (for 4 values of lambda)
layout(matrix(1:4, nrow=2, ncol=2))

# for each lambda, do the entire procedure
for (lambda in lambda_vals){
    # define the random sample for exp distribution at current lambda
    data = rexp(n,lambda)

    # MoM estimate
    mome <- 1/mean(data)

    # calculate MLE for 3 different starting values
    mle1 <- nlminb(start=mome,objective=likelihood,d=data,hessian=T,lower=0,upper=Inf)
    mle2 <- nlminb(start=0.1,objective=likelihood,d=data,hessian=T,lower=0,upper=Inf)
    mle3 <- nlminb(start=0.2,objective=likelihood,d=data,hessian=T,lower=0,upper=Inf)

    cat("Estimates for lambda = ",lambda,"\n")
    cat("MoM Estimate = ", mome,"\n")
    cat("MLE using start value = ",mome,": ", mle1$par,"\n")
    cat("MLE using start value = 0.1: ", mle2$par,"\n")
    cat("MLE using start value = 0.2: ", mle3$par,"\n\n")

    # plot the graphs over the entire range of data for lambda
    p.values <- seq(0.1,4,by=0.01)

    # compute likelihood of each value in p.values and plot against
    # the current value of lambda
    ll.exp <- -1*likelihood(data,p.values)

    plot(p.values,ll.exp,type="l",xlab="Lambda",ylab="Log-Likelihood",
        main = "Log-Likelihood v/s Lambda plot",xlim=c(0.1,4.1))
}