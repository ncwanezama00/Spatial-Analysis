install.packages(c("spatstat"))#install packages
library(spatstat)
load("C:/Users/201807729/OneDrive - University of Fort Hare/Documents/Practical 3/ppa.RData")
marks(starbucks) <- NULL
Window(starbucks) <- ma
plot(starbucks, main=NULL, cols=rgb(0,0,0,.2), pch=20)#plot starbucks
hist(pop, main=NULL, las=1)
pop.lg <- log(pop)#plot lg
hist(pop.lg, main=NULL, las=1)#histogram
#Task 2 density based analysis
Q <- quadratcount(starbucks, nx= 6, ny=3)
plot(starbucks, pch=20, cols="grey70", main=NULL)
plot(Q, add=TRUE)
Q.d <- intensity(Q)
plot(intensity(Q, image=TRUE), main=NULL, las=1)#plot intensity
plot(starbucks, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)
starbucks.km <- rescale(starbucks, 1000, "km")
ma.km <- rescale(ma, 1000, "km")
pop.km <- rescale(pop, 1000, "km")
pop.lg.km <- rescale(pop.lg, 1000, "km")
Q <- quadratcount(starbucks.km, nx= 6, ny=3)
Q.d <- intensity(Q)
plot(intensity(Q, image=TRUE), main=NULL, las=1)
plot(starbucks.km, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)
#Task 3 quadrat dcensity
brk <- c( -Inf, 4, 6, 8 , Inf)
Zcut <- cut(pop.lg.km, breaks=brk, labels=1:4)
E <- tess(image=Zcut)
plot(E, main="", las=1)#plot E
Q <- quadratcount(starbucks.km, tess = E)#quad count
Q.d <- intensity(Q)
Q.d
plot(intensity(Q, image=TRUE), las=1, main=NULL)#plot q
plot(starbucks.km, pch=20, cex=0.6, col=rgb(1,1,1,.5), add=TRUE)#plot starbucks
cl <- interp.colours(c("blue", "green", "red"), E$n)
plot( intensity(Q, image=TRUE), las=1, col=cl, main=NULL)
plot(starbucks.km, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)
#task 4 kernel density
K1 <- density(starbucks.km)#k1
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)
K2 <- density(starbucks.km, sigma=50)#k2 plot
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)
K3 <- density(starbucks.km, kernel = "disc", sigma=50)#k3
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)#interval
rho <- rhohat(starbucks.km, pop.lg.km, method="ratio")
plot(rho, las=1, main=NULL, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))#plot
pred <- predict(rho)#predict
cl <- interp.colours(c("gold","pink","green"), 100)
plot(pred, col=cl, las=1, main=NULL, gamma = 0.25)#plot
K1_vs_pred <- pairs(K1, pred, plot = FALSE)#k1 vs pred
plot(K1_vs_pred$pred ~ K1_vs_pred$K1, pch=20,
     xlab = "Observed intensity",
     ylab = "Predicted intensity",
     col = rgb(0,0,0,0.1))
K1_vs_K1 <- pairs(K1, K1, labels = c("K1a", "K1b"), plot = FALSE)
plot(K1_vs_K1$K1a ~ K1_vs_K1$K1b, pch=20,
     xlab = "Observed intensity",
     ylab = "Observed intensity")
summary(as.data.frame(K1_vs_pred))#summary
plot(K1_vs_pred$pred ~ K1_vs_pred$K1, pch=20,
     xlab = "Observed intensity",
     ylab = "Predicted intensity",
     col = rgb(0,0,0,0.1),
     xlim = c(0, 0.04), ylim = c(0, 0.1))
abline(a=0, b = 1, col = "red")#abline
PPM1 <- ppm(starbucks.km ~ pop.lg.km)
plot(effectfun(PPM1, "pop.lg.km", se.fit=TRUE), main=NULL,
     las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))
PPM1
#task 5 disatnce based analysis
mean(nndist(starbucks.km, k=1))
ANN <- apply(nndist(starbucks.km, k=1:100),2,FUN=mean)
plot(ANN ~ eval(1:100), type="b", main=NULL, las=1)#plot
K <- Kest(starbucks.km)
plot(K, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))#k plot
L <- Lest(starbucks.km, main=NULL)
plot(L, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))#l plot
plot(L, . -r ~ r, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))
g <- pcf(starbucks.km)
plot(g, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))
#task 6 hypothesis tests
ann.p <- mean(nndist(starbucks.km, k=1))
ann.p
n <- 599L
ann.r <- vector(length = n)
for (i in 1:n){
  rand.p <- rpoint(n=starbucks.km$n, win=ma.km)
  ann.r[i] <- mean(nndist(rand.p, k=1))
}
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))#plot
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")#abline
n <- 599L
ann.r <- vector(length=n)
for (i in 1:n){
  rand.p <- rpoint(n=starbucks.km$n, f=pop.km)
  ann.r[i] <- mean(nndist(rand.p, k=1))
}
Window(rand.p) <- ma.km
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")
#task 7 pseudo p-value
N.greater <- sum(ann.r > ann.p)
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1)
p
#task 8 test for a poison point 
PPM1 <- ppm(starbucks.km ~ pop.lg.km)
PPM1
PPM0 <- ppm(starbucks.km ~ 1)
PPM0
starbucks.km$n / area(ma.km)
anova(PPM0, PPM1, test="LRT")

