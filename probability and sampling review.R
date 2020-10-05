par(mfrow=c(1,1))
curve(dnorm(x),
      xlim = c(-3.5, 3.5),
      ylab = "Density", 
      main = "Standard Normal Density Function") 

# compute density at x=-1.96, x=0 and x=1.96
dnorm(x = c(-1.96, 0, 1.96))
#> [1] 0.05844094 0.39894228 0.05844094

curve(pnorm(x), 
      xlim = c(-3.5, 3.5), 
      ylab = "Probability", 
      main = "Standard Normal Cumulative Distribution Function")

# define the standard normal PDF as an R function
f <- function(x) {
  1/(sqrt(2 * pi)) * exp(-0.5 * x^2)
}

# define a vector of reals
quants <- c(-1.96, 0, 1.96)

# compute densities
f(quants)
#> [1] 0.05844094 0.39894228 0.05844094

# compare to the results produced by 'dnorm()'
f(quants) == dnorm(quants)
#> [1] TRUE TRUE TRUE

# integrate f()
integrate(f, 
          lower = -Inf, 
          upper = 1.337)
#> 0.9093887 with absolute error < 1.7e-07

# compute the probability using pnorm()
pnorm(1.337)
#> [1] 0.9093887

# compute the probability
1 - 2 * (pnorm(-1.96)) 
#> [1] 0.9500042

pnorm(4, mean = 5, sd = 5) - pnorm(3, mean = 5, sd = 5) 

# plot the PDF
curve(dchisq(x, df = 3), 
      xlim = c(0, 10), 
      ylim = c(0, 1), 
      col = "blue",
      ylab = "",
      main = "p.d.f. and c.d.f of Chi-Squared Distribution, M = 3")

# add the CDF to the plot
curve(pchisq(x, df = 3), 
      xlim = c(0, 10), 
      add = TRUE, 
      col = "red")

# add a legend to the plot
legend("topleft", 
       c("PDF", "CDF"), 
       col = c("blue", "red"), 
       lty = c(1, 1))

# plot the density for M=1
curve(dchisq(x, df = 1), 
      xlim = c(0, 15), 
      xlab = "x", 
      ylab = "Density", 
      main = "Chi-Square Distributed Random Variables")

# add densities for M=2,...,7 to the plot using a 'for()' loop 
for (M in 2:7) {
  curve(dchisq(x, df = M),
        xlim = c(0, 15), 
        add = T, 
        col = M)
}

# add a legend
legend("topright", 
       as.character(1:7), 
       col = 1:7 , 
       lty = 1, 
       title = "D.F.")

# plot the standard normal density
curve(dnorm(x), 
      xlim = c(-4, 4), 
      xlab = "x", 
      lty = 2, 
      ylab = "Density", 
      main = "Densities of t Distributions")

# plot the t density for M=2
curve(dt(x, df = 2), 
      xlim = c(-4, 4), 
      col = 2, 
      add = T)

# plot the t density for M=4
curve(dt(x, df = 4), 
      xlim = c(-4, 4), 
      col = 3, 
      add = T)

# plot the t density for M=25
curve(dt(x, df = 25), 
      xlim = c(-4, 4), 
      col = 4, 
      add = T)

# add a legend
legend("topright", 
       c("N(0, 1)", "M=2", "M=4", "M=25"), 
       col = 1:4, 
       lty = c(2, 1, 1, 1))

pf(2, df1 = 3, df2 = 14, lower.tail = F)
#> [1] 0.1603538

# define coordinate vectors for vertices of the polygon
x <- c(2, seq(2, 10, 0.01), 10)
y <- c(0, df(seq(2, 10, 0.01), 3, 14), 0)

# draw density of F_{3, 14}
curve(df(x ,3 ,14), 
      ylim = c(0, 0.8), 
      xlim = c(0, 10), 
      ylab = "Density",
      main = "Density Function")

# draw the polygon
polygon(x, y, col = "orange")

sum(sample(1:6, 2, replace = T))

# Vector of outcomes
S <- 2:12

# Vector of probabilities
PS <- c(1:6, 5:1) / 36

# Expectation of S
ES <- sum(S * PS)
ES
#> [1] 7

# Variance of S
VarS <- sum((S - c(ES))^2 * PS)
VarS
#> [1] 5.833333

# divide the plotting area into one row with two columns
par(mfrow = c(1, 2))

# plot the distribution of S
barplot(PS, 
        ylim = c(0, 0.2), 
        xlab = "S", 
        ylab = "Probability", 
        col = "steelblue", 
        space = 0, 
        main = "Sum of Two Dice Rolls")

# plot the distribution of D 
probability <- rep(1/6, 6)
names(probability) <- 1:6

barplot(probability, 
        ylim = c(0, 0.2), 
        xlab = "D", 
        col = "steelblue", 
        space = 0, 
        main = "Outcome of a Single Dice Roll")

# set sample size and number of samples
n <- 10
reps <- 10000

# perform random sampling
samples <- replicate(reps, rnorm(n)) # 10 x 10000 sample matrix

# compute sample means
sample.avgs <- colMeans(samples)

# check that 'sample.avgs' is a vector
is.vector(sample.avgs) 
#> [1] TRUE

# print the first few entries to the console
head(sample.avgs)
#> [1] -0.1045919  0.2264301  0.5308715 -0.2243476  0.2186909  0.2564663

par(mfrow = c(1, 1))
# Plot the density histogram
hist(sample.avgs, 
     ylim = c(0, 1.4), 
     col = "steelblue" , 
     freq = F, 
     breaks = 20)

# overlay the theoretical distribution of sample averages on top of the histogram
curve(dnorm(x, sd = 1/sqrt(n)), 
      col = "red", 
      lwd = "2", 
      add = T)

# number of repetitions
reps <- 10000

# set degrees of freedom of a chi-Square Distribution
DF <- 3 

# sample 10000 column vectors à 3 N(0,1) R.V.S
Z <- replicate(reps, rnorm(DF)) 

# column sums of squares
X <- colSums(Z^2)

# histogram of column sums of squares
hist(X, 
     freq = F, 
     col = "steelblue", 
     breaks = 40, 
     ylab = "Density", 
     main = "")

# add theoretical density
curve(dchisq(x, df = DF), 
      type = 'l', 
      lwd = 2, 
      col = "red", 
      add = T)

# set seed
set.seed(1)

# set number of coin tosses and simulate
N <- 30000
Y <- sample(0:1, N, replace = T)

# Calculate R_n for 1:N
S <- cumsum(Y)
R <- S/(1:N)

# Plot the path.
plot(R, 
     ylim = c(0.3, 0.7), 
     type = "l", 
     col = "steelblue", 
     lwd = 2, 
     xlab = "n", 
     ylab = "R_n",
     main = "Converging Share of Heads in Repeated Coin Tossing")

# Add a dashed line for R_n = 0.5
lines(c(0, N), 
      c(0.5, 0.5), 
      col = "darkred", 
      lty = 2, 
      lwd = 1)

# subdivide the plot panel into a 2-by-2 array
par(mfrow = c(2, 2))

# set the number of repetitions and the sample sizes
reps <- 10000
sample.sizes <- c(5, 20, 75, 100)

# set seed for reproducibility
set.seed(123)

# outer loop (loop over the sample sizes)
for (n in sample.sizes) {
  
  samplemean <- rep(0, reps) #initialize the vector of sample means
  stdsamplemean <- rep(0, reps) #initialize the vector of standardized sample means
  
  # inner loop (loop over repetitions)   
  for (i in 1:reps) {
    x <- rbinom(n, 1, 0.5)
    samplemean[i] <- mean(x)
    stdsamplemean[i] <- sqrt(n)*(mean(x) - 0.5)/0.5
  }
  
  # plot histogram and overlay the N(0,1) density in every iteration    
  hist(stdsamplemean, 
       col = "steelblue", 
       freq = FALSE, 
       breaks = 40,
       xlim = c(-3, 3), 
       ylim = c(0, 0.8), 
       xlab = paste("n =", n), 
       main = "")
  
  curve(dnorm(x), 
        lwd = 2, 
        col = "darkred", 
        add = TRUE)
}  

7. Normal Distribution II
Let  
Y
???
N
(
  2
  ,
  12
)
.

#Instructions:
  
#Generate  
#10 random numbers from this distribution.