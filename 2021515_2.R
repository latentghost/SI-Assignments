library(dplyr)

rm(list=ls())

# read the .csv file
readdata <- read.csv("./data.csv")

# remove the Index column from the data
data <- select(readdata,x)

# define a function that returns the -ve log-likelihood
# of a given input data and given values of mu and sigma
likelihood <- function(d, params){
    mu <- params[1]
    sig <- params[2]
    n <- length(d)
    return (-(-n/2*log(2*pi) - n/2*log(sig) - 1/(2*sig)*(sum((d - mu)^2))))
}

# define a function that calculates the MLE for a normal distribution
mle.normal <- function(d){
    # initially, estimate mu and sigma by MoM
    mu = mean(d$x)
    sigma = sd(d$x)

    # maximise the log-likelihood function
    out = nlminb(start=c(mu,sigma), objective=likelihood, d=data$x, hessian=T, lower=c(-Inf,0), upper=c(Inf,Inf))
    return (list(mle_mu = out$par[1], mle_var = out$par[2]))
}


# calculate the MLE for the given data
mle <- mle.normal(data)
mu <- mle$mle_mu
sigma <- mle$mle_var
cat("MLE for the given Data: ", "\n", "   mu = ",mu, "\n","   sigma^2 = ",sigma,"\n")


# assuming mu to be known, plotting likelihood function against
# different values of sigma
p.values <- seq(sigma-10,sigma,by=0.01)
n <- length(data)
ll.norm <- (-n/2*log(2*pi) - n/2*log(p.values) - 1/(2*p.values)*(sum((data - mu)^2)))

plot(p.values,ll.norm,xlab="Variance",ylab="Log-Likelihood",
    main="Log-Likelihood v/s Variance",xlim=c(sigma-10,sigma+1))


# define an exponential distribution on -mu
# (equivalent to exp distribution on 1)
newdist <- rexp(1000,1)

# likelihood function for exponential distribution
explikelihood <- function(d,l){
    return (-(n*log(l) - l*(sum(d))))
}

# compute the MLE for the distribution
mle_exp <- nlminb(start=10,objective=explikelihood,d=newdist,hessian=T,lower=0,upper=Inf)

cat("\nMLE for exp(-μ): ", mle_exp$par,"\n")