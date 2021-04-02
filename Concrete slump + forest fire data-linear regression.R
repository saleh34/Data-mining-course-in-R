library(openxlsx)
cstd <- read.xlsx("Concrete Slump Test Data.xlsx")
cstdm <- as.data.frame(cstd[,c("Cement","Slag", "Fly.Ash", "Water","SP", "Coarse.Aggregate", "Fine.Aggregate","Slump.Flow")])
library(car)
cor(cstdm)
scatterplotMatrix(cstdm, spread= FALSE, lty.smooth=2, main="scatter plot matrix")
fit <- lm(Slump.Flow ~ Cement + Slag + Fly.Ash + Water + SP + Coarse.Aggregate + Fine.Aggregate, data=cstdm)
summary(fit)
fit <- lm(Slump.Flow ~ Water + SP + Fine.Aggregate + Coarse.Aggregate, data=cstdm)
summary(fit)
fit2 <- lm(Slump.Flow ~ Water + SP + Fine.Aggregate , data=cstdm)
summary(fit2)
fit3 <- lm(Slump.Flow ~ SP + Fine.Aggregate + Coarse.Aggregate, data=cstdm)
summary(fit3)
fit4 <- lm(Slump.Flow ~ Water + Fine.Aggregate + Coarse.Aggregate, data=cstdm)
summary(fit4)
fit5 <- lm(Slump.Flow ~ Water + SP + Coarse.Aggregate, data=cstdm)
summary(fit5)
fit6 <- lm(Slump.Flow ~ Water, data=cstdm)
summary(fit6)
fit7 <- lm(Slump.Flow ~ Water + Fine.Aggregate + Coarse.Aggregate + Water:Fine.Aggregate + Water:Coarse.Aggregate + Fine.Aggregate:Coarse.Aggregate + Water:Fine.Aggregate:Coarse.Aggregate,data=cstdm)
summary(fit7)
fit8 <- lm(Slump.Flow ~ Water + I(Water^2),data=cstdm)
summary(fit8)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(2,2))
plot(fit3)
par(mfrow=c(2,2))
plot(fit4)
par(mfrow=c(2,2))
plot(fit5)
qqPlot(fit, labels=row.names(cstdm),id.method="identify",simulate=TRUE, main= "Q-Q PLOT")
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit) 
  hist(z, breaks=nbreaks,freq=FALSE,
       xlab="Studentized residual",
       main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2) 
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2, col=c("blue","red"),cex=.7)
  }
residplot(fit)
residplot <- function(fit2, nbreaks=10) {
  z <- rstudent(fit2) 
  hist(z, breaks=nbreaks,freq=FALSE,
       xlab="Studentized residual",
       main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2) 
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2, col=c("blue","red"),cex=.7)
}
residplot(fit2)
residplot <- function(fit3, nbreaks=10) {
  z <- rstudent(fit3) 
  hist(z, breaks=nbreaks,freq=FALSE,
       xlab="Studentized residual",
       main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2) 
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2, col=c("blue","red"),cex=.7)
}
residplot(fit3)
residplot <- function(fit4, nbreaks=10) {
  z <- rstudent(fit4) 
  hist(z, breaks=nbreaks,freq=FALSE,
       xlab="Studentized residual",
       main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2) 
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2, col=c("blue","red"),cex=.7)
}
residplot(fit4)
residplot <- function(fit5, nbreaks=10) {
  z <- rstudent(fit5) 
  hist(z, breaks=nbreaks,freq=FALSE,
       xlab="Studentized residual",
       main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2) 
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2, col=c("blue","red"),cex=.7)
}
residplot(fit5)
library(car)
durbinWatsonTest(fit)
durbinWatsonTest(fit2)
durbinWatsonTest(fit3)
durbinWatsonTest(fit4)
durbinWatsonTest(fit5)
library(car)
crPlots(fit)
crPlots(fit2)
crPlots(fit3)
crPlots(fit4)
crPlots(fit5)
ncvTest(fit)
ncvTest(fit2)
ncvTest(fit3)
ncvTest(fit4)
ncvTest(fit5)
spreadLevelPlot(fit)
spreadLevelPlot(fit2)
spreadLevelPlot(fit3)
spreadLevelPlot(fit4)
spreadLevelPlot(fit5)
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)
gvmodel2 <- gvlma(fit2)
summary(gvmodel2)
gvmodel3 <- gvlma(fit3)
summary(gvmodel3)
gvmodel4 <- gvlma(fit4)
summary(gvmodel4)
gvmodel5 <- gvlma(fit5)
summary(gvmodel5)
vif(fit)
sqrt(vif(fit)) > 2
vif(fit2)
sqrt(vif(fit2)) >2
vif(fit3)
sqrt(vif(fit3)) > 2
vif(fit4)
sqrt(vif(fit4)) >2
vif(fit5)
sqrt(vif(fit5)) >2
outlierTest(fit)
outlierTest(fit2)
outlierTest(fit3)
outlierTest(fit4)
outlierTest(fit5)
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)
hat.plot <- function(fit2) {
  p <- length(coefficients(fit2))
  n <- length(fitted(fit2))
  plot(hatvalues(fit2), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit2), names(hatvalues(fit2)))
}
hat.plot(fit2)
hat.plot <- function(fit3) {
  p <- length(coefficients(fit3))
  n <- length(fitted(fit3))
  plot(hatvalues(fit3), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit3), names(hatvalues(fit3)))
}
hat.plot(fit3)
hat.plot <- function(fit4) {
  p <- length(coefficients(fit4))
  n <- length(fitted(fit4))
  plot(hatvalues(fit4), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit4), names(hatvalues(fit4)))
}
hat.plot(fit4)
hat.plot <- function(fit5) {
  p <- length(coefficients(fit5))
  n <- length(fitted(fit5))
  plot(hatvalues(fit5), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit5), names(hatvalues(fit5)))
}
hat.plot(fit5)
cutoff <- 4/(nrow(cstdm)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(cstdm)-length(fit2$coefficients)-2)
plot(fit2, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(cstdm)-length(fit3$coefficients)-2)
plot(fit3, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(cstdm)-length(fit4$coefficients)-2)
plot(fit4, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(cstdm)-length(fit5$coefficients)-2)
plot(fit5, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
influencePlot(fit, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to cook's distance")
influencePlot(fit2, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to cook's distance")
influencePlot(fit3, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to cook's distance")
influencePlot(fit4, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to cook's distance")
influencePlot(fit5, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to cook's distance")
cstdm <- cstdm[-c(4,7,8,14,69,79),]
library(car)
summary(powerTransform(cstdm$Slump.Flow))
boxTidwell(Slump.Flow ~ Water + SP, data=cstdm)
library(leaps)
leaps <- regsubsets(Slump.Flow ~ Water + SP + Fine.Aggregate + Coarse.Aggregate, data=cstdm, nbest=2)
plot(leaps, scale="adjr2")
library(car)
subsets(leaps, statistic="cp", 
        main="Cp plot for all subsets regression")
abline(1,1,lty=2,col="red")
zcstdm <- as.data.frame(scale(cstdm)) 
zfit <- lm(Slump.Flow ~ Water + SP + Coarse.Aggregate + Fine.Aggregate,data=zcstdm)
coef(zfit)
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
rxx <- R[2:nvar, 2:nvar]
rxy <- R[2:nvar,1]
svd <- eigen(rxx)
evec <- svd$vectors
ev <- svd$values
delta <- diag(sqrt(ev))
lambda <- evec %*% delta %*% t(evec)
lambdasq <- lambda ^ 2
beta <-solve(lambda) %*% rxy
rsquare <- colSums(beta ^ 2)
rawwgt <- lambdasq %*% beta ^ 2
import <- (rawwgt / rsquare) * 100
lbls <- names(fit$model[2:nvar])
rownames(import) <- lbls
colnames(import) <- "Weights"
barplot(t(import),names.arg=lbls,
        ylab="% of R-square",
        xlab="Predictor Variables",
        main="Relative importance of predictor variables",
        sub=paste("R-Square=", round(rsquare, digits=3)),
        ...)
return(import)
}
fit <- lm(Slump.Flow ~ Water + SP + Fine.Aggregate + Coarse.Aggregate,data = cstdm)
relweights(fit,col="lightgrey")




ffd <- read.xlsx("Forest Fires Data.xlsx")

ffdm <- as.data.frame(ffd[,c("X", "Y", "FFMC", "DMC", "DC", "ISI", "Temp", "RH", "Wind", "Rain", "Area")])

library(car)
cor(ffdm)
scatterplotMatrix(ffdm, spread= FALSE, lty.smooth=2, main="scatter plot matrix")
fit_1 <- lm(Area ~ X + Y + FFMC + DMC + DC + ISI + Temp + RH + Wind + Rain, data=ffdm)
summary(fit_1)
fit_2 <- lm(Area ~ X + Temp + Rain + Wind, data=ffdm)
summary(fit_2)
fit_3 <- lm(Area ~ X + Temp + Rain, data=ffdm)
summary(fit_3)
fit_4 <- lm(Area ~ X + Rain + Wind, data=ffdm)
summary(fit_4)
fit_5 <- lm(Area ~ X + Temp + Wind, data=ffdm)
summary(fit_5)
fit_6 <- lm(Area ~ Temp + Wind + Rain, data=ffdm)
summary(fit_6)
fit_7 <- lm(Area~ X + Temp + X:Temp, data=ffdm)
summary(fit_7)
par(mfrow=c(2,2))
plot(fit_1)
plot(fit_2)
plot(fit_3)
plot(fit_4)
plot(fit_5)
plot(fit_6)
qqPlot(fit, labels=row.names(ffdm),id.method="identify",simulate=TRUE, main= "Q-Q PLOT")
residplot <- function(fit_2, nbreaks=10) {
  z <- rstudent(fit_2) 
  hist(z, breaks=nbreaks,freq=FALSE,
       xlab="Studentized residual",
       main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2) 
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2, col=c("blue","red"),cex=.7)
}
residplot(fit_2)

residplot <- function(fit_3, nbreaks=10) {
  z <- rstudent(fit_3) 
  hist(z, breaks=nbreaks,freq=FALSE,
       xlab="Studentized residual",
       main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2) 
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2, col=c("blue","red"),cex=.7)
}
residplot(fit_3)

residplot <- function(fit_4, nbreaks=10) {
  z <- rstudent(fit_4) 
  hist(z, breaks=nbreaks,freq=FALSE,
       xlab="Studentized residual",
       main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2) 
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2, col=c("blue","red"),cex=.7)
}
residplot(fit_4)

residplot <- function(fit_5, nbreaks=10) {
  z <- rstudent(fit_5) 
  hist(z, breaks=nbreaks,freq=FALSE,
       xlab="Studentized residual",
       main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2) 
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2, col=c("blue","red"),cex=.7)
}
residplot(fit_5)

residplot <- function(fit_6, nbreaks=10) {
  z <- rstudent(fit_6) 
  hist(z, breaks=nbreaks,freq=FALSE,
       xlab="Studentized residual",
       main="Distribution of errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue",lwd=2) 
  lines(density(z)$x,density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",
         legend=c("Normal Curve","Kernel Density Curve"),
         lty=1:2, col=c("blue","red"),cex=.7)
}
residplot(fit_6)

library(car)
durbinWatsonTest(fit_1)
durbinWatsonTest(fit_2)
durbinWatsonTest(fit_3)
durbinWatsonTest(fit_4)
durbinWatsonTest(fit_5)
durbinWatsonTest(fit_6)
library(car)
crPlots(fit_1)
crPlots(fit_2)
crPlots(fit_3)
crPlots(fit_4)
crPlots(fit_5)
crPlots(fit_6)
ncvTest(fit_1)
ncvTest(fit_2)
ncvTest(fit_3)
ncvTest(fit_4)
ncvTest(fit_5)
ncvTest(fit_6)
spreadLevelPlot(fit_1)
spreadLevelPlot(fit_2)
spreadLevelPlot(fit_3)
spreadLevelPlot(fit_4)
spreadLevelPlot(fit_5)
library(gvlma)
gvmodel_1 <- gvlma(fit_1)
summary(gvmodel_1)
gvmodel_2 <- gvlma(fit_2)
summary(gvmodel_2)
gvmodel_3 <- gvlma(fit_3)
summary(gvmodel_3)
gvmodel_4 <- gvlma(fit_4)
summary(gvmodel_4)
gvmodel_5 <- gvlma(fit_5)
summary(gvmodel_5)
gvmodel_6 <- gvlma(fit_6)
summary(gvmodel_6)
vif(fit_1)
sqrt(vif(fit_1)) > 2
vif(fit_2)
sqrt(vif(fit_2)) >2
vif(fit_3)
sqrt(vif(fit_3)) > 2
vif(fit_4)
sqrt(vif(fit_4)) >2
vif(fit_5)
sqrt(vif(fit_5)) >2
vif(fit_6)
sqrt(vif(fit_6)) >2
outlierTest(fit_1)
outlierTest(fit_2)
outlierTest(fit_3)
outlierTest(fit_4)
outlierTest(fit_5)
outlierTest(fit_6)
ffd_fake <- ffdm
hat.plot <- function(fit_1) {
  p <- length(coefficients(fit_1))
  n <- length(fitted(fit_1))
  plot(hatvalues(fit_1), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit_1), names(hatvalues(fit_1)))
}
hat.plot(fit_1)
hat.plot <- function(fit_2) {
  p <- length(coefficients(fit_2))
  n <- length(fitted(fit_2))
  plot(hatvalues(fit_2), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit_2), names(hatvalues(fit_2)))
}
hat.plot(fit_2)
hat.plot <- function(fit_3) {
  p <- length(coefficients(fit_3))
  n <- length(fitted(fit_3))
  plot(hatvalues(fit_3), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit_3), names(hatvalues(fit_3)))
}
hat.plot(fit_3)
hat.plot <- function(fit_4) {
  p <- length(coefficients(fit_4))
  n <- length(fitted(fit_4))
  plot(hatvalues(fit_4), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit_4), names(hatvalues(fit_4)))
}
hat.plot(fit_4)
hat.plot <- function(fit_5) {
  p <- length(coefficients(fit_5))
  n <- length(fitted(fit_5))
  plot(hatvalues(fit_5), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit_5), names(hatvalues(fit_5)))
}
hat.plot(fit_5)
hat.plot <- function(fit_6) {
  p <- length(coefficients(fit_6))
  n <- length(fitted(fit_6))
  plot(hatvalues(fit_6), main="index plot of hat values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit_6), names(hatvalues(fit_6)))
}
hat.plot(fit_6)
cutoff <- 4/(nrow(cstdm)-length(fit_1$coefficients)-2)
plot(fit_1, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(cstdm)-length(fit_2$coefficients)-2)
plot(fit_2, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(cstdm)-length(fit_3$coefficients)-2)
plot(fit_3, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(cstdm)-length(fit_4$coefficients)-2)
plot(fit_4, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(cstdm)-length(fit_5$coefficients)-2)
plot(fit_5, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(cstdm)-length(fit_6$coefficients)-2)
plot(fit_6, which=4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
ffdm <- ffdm[-c(23,238,239,300,380,500,510,239,416,466,476,480),]
library(car)
summary(powerTransform(ffdm$Area,family="yjPower"))
library(leaps)
leaps <- regsubsets(Area ~ X + Temp + Rain + Wind, data=ffdm, nbest=2)
plot(leaps, scale="adjr2")
library(car)
subsets(leaps, statistic="cp", 
        main="Cp plot for all subsets regression")
abline(1,1,lty=2,col="red")
zffdm <- as.data.frame(scale(ffdm)) 
zfit <- lm(Area ~ X + Temp + Rain + Wind,data=zffdm)
coef(zfit)
relweights <- function(fit_2,...){
  R <- cor(fit_2$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar,1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <-solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  lbls <- names(fit_2$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import),names.arg=lbls,
          ylab="% of R-square",
          xlab="Predictor Variables",
          main="Relative importance of predictor variables",
          sub=paste("R-Square=", round(rsquare, digits=3)),
          ...)
  return(import)
}
fit_2 <- lm(Area~ X + Temp + Rain + Wind,data = ffdm)
relweights(fit_2,col="lightgrey")
