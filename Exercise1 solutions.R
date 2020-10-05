#Exercise solutions
#1. 
set.seed(123)
# draw the winning numbers for this week
sample(1:49,size = 6)

#2
# define the PDF
f <- function(x){x/4*exp(-x^2/8)}

# integrate f over the domain
integrate(f, 0, Inf)$value

#3
# define the function ex
ex <- function(x){x*f(x)}

# compute the expected value of X
expected_value <- integrate(ex, 0, Inf)$value
expected_value

# define the function ex2
ex2 <- function(x){x^2*f(x)}

# compute the variance of X
variance <- integrate(ex2, 0, Inf)$value - expected_value^2
variance

#4
set.seed(123)
# generate 10 random numbers from the given distribution.
rnorm(10, mean = 2, sd = sqrt(12))

#5 
# define a function for the estimator
Y_tilde <- function(x){sum(x)/(length(x)-1)}

# repeatedly compute estimates and store the results in est_biased
set.seed(123)
est_biased <- replicate(n = 10000, expr = Y_tilde(rnorm(5, 10, 5)))

# plot a histogram of est_biased
hist(est_biased)

# add a red vertical line at mu = 10
abline(v = 10, col = "red")