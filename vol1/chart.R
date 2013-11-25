png(file="ch4_sinwave.png",width=512,height=256)
plot(sin,xlim=c(-10,10),ylim=c(-1,1))
dev.off()

png(file="ch4_runif.png",width=512,height=256)
hist(rnorm(100000),breaks=seq(-5,5,by=0.1))
dev.off()

png(file="ch4_runif.png",width=512,height=256)
hist(runif(1000000),breaks=seq(0,1,by=0.01))
dev.off()

png(file="ch5_iris.png",width=512,height=512)
iris.kmeans <- kmeans(iris[, -5], 3, iter.max = 1000)
iris.dist <- dist(iris[, -5])
iris.mds <- cmdscale(iris.dist)
ideal.chars <- c("*", "o", "+")[as.integer(iris$Species)]
actual.colors <- rainbow(3)[iris.kmeans$cluster]
plot(iris.mds, col = actual.colors, pch = ideal.chars, xlab = "X", ylab = "Y")
dev.off()

mexFunct <- function(t) (1 - t^2) * exp(-t^2 / 2)
png(file="ch7_mex2d.png",width=512,height=256)
plot(mexFunct,xlim=c(-5,5),ylim=c(-0.5,1))
dev.off()

rbfFunct <- function(x) exp( (-x^2)/8)
png(file="ch7_gaus2d.png",width=512,height=256)
plot(rbfFunct,xlim=c(-5,5),ylim=c(0,1))
dev.off()

mexFunct <- function(t) (1 - t^2) * exp(-t^2 / 2)
png(file="ch7_mex2d.png",width=512,height=256)
plot(mexFunct,xlim=c(-5,5),ylim=c(-0.5,1))
dev.off()

stemp <- 1000
etemp <- 10
kmax <- 500
cool <- function(k) stemp*((etemp/stemp)^(k/kmax))
png(file="ch8_cool.png",width=512,height=256)
plot(cool,xlim=c(0,500),ylim=c(0,1000))
dev.off()

library('Cairo')

png(file="ch9_simplex.png",width=512,height=256,type="cairo")

Cairo(file="ch9_simplex.png", 
      bg="white",
      type="png",
      units="in", 
      width=5, 
      height=4, 
      pointsize=12, 
      dpi=72)
library(calibrate)
x <- seq(-6, -2, .01)  # Set grid points for x
y <- seq(-6, -2, .01)	# Set grid points for y
z <- outer(x, y, FUN=function(xx,yy) { (((xx^2)+yy-11)^2) + ((xx+(yy^2)-7)^2) })  # Compute function at all grid points
par(mar=c(2,2,0,0)+0.1)
contour(x, y, z)	# Make contour plot
x=c(-3, -5, -5.8, -3)
y=c(-5.5, -5, -5.8, -5.5)
labs = c(expression('x'[s]),expression('x'[l]),expression('x'[h]))
s=1:3
segments(x[s], y[s], x[s+1], y[s+1], col = "black", lwd=2)
textxy(x[s]+0.2, y[s], labs, cx=1.5)
dev.off()

Cairo(file="ch10_linear.png", 
      bg="white",
      type="png",
      units="in", 
      width=5, 
      height=4, 
      pointsize=12, 
      dpi=72)
linFunct <- function(x) ( 0.5*x + 2 )
png(file="ch10_linear.png",width=512,height=256)
plot(linFunct,xlim=c(-5,5),ylim=c(-5,5))
dev.off()





svg("Anscombe's quartet 3.svg", width=11, height=8)
 op <- par(las=1, mfrow=c(2,2), mar=1.5+c(4,4,1,1), oma=c(0,0,0,0),
           lab=c(6,6,7), cex.lab=2.0, cex.axis=1.3, mgp=c(3,1,0))
 ff <- y ~ x
 for(i in 1:4) {
   ff[[2]] <- as.name(paste("y", i, sep=""))
   ff[[3]] <- as.name(paste("x", i, sep=""))
   lmi <- lm(ff, data= anscombe)
   xl <- substitute(expression(x[i]), list(i=i))  
   yl <- substitute(expression(y[i]), list(i=i))
   plot(ff, data=anscombe, col="red", pch=21, cex=2.4, bg = "orange", 
        xlim=c(3,19), ylim=c(3,13)
        , xlab=eval(xl), ylab=yl  # for version 3
       )  
   abline(lmi, col="blue")
 }
 par(op)
 dev.off()


Cairo(file="ch10_logit.png", 
      bg="white",
      type="png",
      units="in", 
      width=5, 
      height=4, 
      pointsize=12, 
      dpi=72)
logitFunct <- function(x) ( 1/(1+exp(x)) )
plot(logitFunct,xlim=c(-6,6),ylim=c(0,1))
dev.off()

