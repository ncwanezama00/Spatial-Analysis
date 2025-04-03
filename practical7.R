#simulate standard Normal random numbers with mean 0 and standard deviation 1.
x <- rnorm(10)
x
#simulate numbers with mean and standard deviation
x <- rnorm(10, mean = 0, sd = 1)
x
summary(x)
#pnorm() function
pnorm(2)
#Task 2: Setting the random number seed
set.seed(1)
rnorm(5)
rnorm(10)
rpois(10, lambda = 5)
#1st set
j <- rnorm(10, mean = 1)
j
# 2nd set
k <- rnorm(10, mean = 2)
k
#3rd set
l <- rnorm(10, mean = 10)
l
#Task 3: Simulating a Linear Model
set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
summary(y)
plot(y)
set.seed(10)
x <- rbinom(100, 1, 0.5)
str(x)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
plot(y)
set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3 * x
y <- rpois(100, exp(log.mu))
summary(y)
plot(y)
#Task 4: Random Sampling
set.seed(1)
sample(1:10, 4)
sample(1:10, 4)
sample(letters, 5)
sample(1:10)
sample(1:10)
sample(1:10, replace = TRUE)
library(datasets)
data(airquality)
head(airquality)
set.seed(20)
idx <- seq_len(nrow(airquality))
samp <- sample(idx, 6)
airquality[samp, ]
