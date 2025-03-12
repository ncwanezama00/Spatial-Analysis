install.packages(c("sf","tmap","spdep"))#install packages
library('sf')
library('tmap')
library('spdep')
s <- readRDS("C:/Users/201807729/Downloads/Practical 4/Prac 4_Data/nhme.rds")
colnames(s)
s$Income
hist(s$Income, main=NULL)#hist
boxplot(s$Income, horizontal = TRUE)
tm_shape(s) + tm_fill(col="Income", style="quantile", n=8, palette="Greens") +
  tm_legend(outside=TRUE)#plot
#Task 2A
nb <- poly2nb(s, queen=TRUE)
nb[1]
#Task 2B
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]
#Task 2C
inc.lag <- lag.listw(lw, s$Income)
inc.lag
plot(inc.lag ~ s$Income, pch=16, asp=1)
M1 <- lm(inc.lag ~ s$Income)
abline(M1, col="blue")
coef(M1)[2]
I <- moran(s$Income, lw, length(nb), Szero(lw))[1]
I
#Task 3A
moran.test(s$Income,lw, alternative="greater")
#Task 3B
MC<- moran.mc(s$Income, lw, nsim=999, alternative="greater")
MC$p.value
hist(MC$res, 
     main = "Monte Carlo Moran's I Simulation", 
     xlab = "Simulated Moran's I", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")
# Add a vertical line to show the observed Moran’s I
abline(v = MC$statistic, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Observed Moran's I"), col = "red", lwd = 2, lty = 2)
set.seed(131)
s$rand1 <- sample(s$Income, length(s$Income), replace = FALSE)
s$rand2 <- sample(s$Income, length(s$Income), replace = FALSE)
s$rand3 <- sample(s$Income, length(s$Income), replace = FALSE)
tm_shape(s) + tm_fill(col=c("Income", "rand1", "rand2", "rand3"),
                      style="quantile", n=8, palette="Reds", legend.show = FALSE) +
  tm_facets( nrow=1)
set.seed(2354)
s <- readRDS("C:/Users/201807729/Downloads/Practical 4/Prac 4_Data/fl_hr80.rds")
nb <- poly2nb(s, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
M1 <- moran.mc(s$HR80, lw, nsim=9999, alternative="greater")
# Plot the Monte Carlo Moran's I simulation results
hist(M1$res, 
     main = "Monte Carlo Simulation of Moran's I", 
     xlab = "Simulated Moran's I", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")

# Add a vertical line to show the observed Moran's I
abline(v = M1$statistic, col = "red", lwd = 2, lty = 2)

# Add a legend
legend("topright", legend = c("Observed Moran's I"), 
       col = "red", lwd = 2, lty = 2)
summary(M1)
plot(M1)
