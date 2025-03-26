 #standard Normal random numbers with mean 0 and standard deviation
x <- rnorm(10)
print(x)
x <- rnorm(10, mean = 0, sd = 1)  
print(x)
summary(x)
pnorm(2)
#Task 2
set.seed(1)#Ensures reproductivity
rnorm(5)
set.seed(1)  # Ensures reproducibility
x <- rnorm(10)  # Generates 10 random numbers from N(0, 1)
print(x)
#generating 3 random numbers
set.seed()  # Ensures reproducibility
z <- rnorm(10, mean = 1)
print(z)
#2nd 
set.seed()  # Ensures reproducibility
y <- rnorm(10, mean = 2)
print(y)
#3rd
set.seed()  # Ensures reproducibility
p <- rnorm(10, mean = 10)
print(p)
#task 3
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
#Task 4 Random SAMPLING
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
