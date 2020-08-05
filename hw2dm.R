install.packages("psych",dependencies = TRUE)
usjudge <- source("US Judge Ratings.dat")
library(psych)
fa.parallel(USJudgeRatings[,-1],fa="pc", n.iter = 100, show.legend = TRUE, main="scree plot with parallel analysis")
abline(h=1,lwd=1,col="green")
pc <- principal(USJudgeRatings[,-1],nfactors = 1,rotate="none")
pc
rc <- principal(USJudgeRatings[,-1],nfactor=1,rotate="varimax")
rc
pc$scores
rc$scores
factor.plot(rc,labels = rownames(rc$loadings))
install.packages("xlsx")
library("openxlsx")
GID <- read.xlsx("Glass Identification Data.xlsx")
library(psych)
fa.parallel(GID[,2:10],fa="pc", n.iter=100, show.legend = TRUE, main="scree plot with parallel analysis")
abline(h=1,lwd=1,col="green")
pc <- principal(GID[,2:10],nfactors=4,rotate = "none")
pc
rc <- principal(GID[,2:10],nfactor=4,rotate = "varimax")
rc
pcscores<-pc$scores
head(pcscores)
rcscores<-rc$scores
head(rcscores)
factor.plot(rc,labels=rownames(rc$loadings))

library(psych)
source("Herman23.cor")
fa.parallel(Harman23.cor$cov,n.obs = Harman23.cor$n.obs, fa="fa", n.iter = 100,show.legend = TRUE, main= "scree plot with factor analysis")
fa <- fa(Harman23.cor$cov, nfactors=2, rotate= "none")
fa
fa.varimax <- fa(Harman23.cor$cov, nfactors=2, rotate="varimax")
fa.varimax
factor.scores(Harman23.cor$cov,fa)
factor.scores(Harman23.cor$cov,fa.varimax)
factor.plot(fa.varimax,labels=rownames(fa.varimax$loadings))
fa.promax <- fa(Harman23.cor$cov, nfactors=2, rotate="promax")
fa.promax
fa.diagram(fa.promax,simple = FALSE)
library(psych)
source("Herman74.cor")
fa.parallel(Harman74.cor$cov,n.obs=Harman74.cor$n.obs, fa="fa",n.iter = 100,show.legend = TRUE,main="scree plot with factor analysis")
fa <- fa(Harman74.cor$cov, nfactors=4, rotate="none")
fa
fa.varimax <- fa(Harman74.cor$cov, nfactors=4, rotate="varimax")
fa.varimax
factor.scores(Harman74.cor$cov,fa)
factor.scores(Harman74.cor$cov,fa.varimax)
factor.plot(fa.varimax,labels=rownames(fa.varimax$""))
fa.promax <- fa(Harman74.cor$cov, nfactors=4, rotate="promax")
fa.promax
fa.diagram(fa.promax,simple=FALSE)
library(openxlsx)
vertebraldata <- read.xlsx("Vertebral Column Data.xlsx",sheet=1)
fa.parallel(vertebraldata[,1:6],fa="pc",n.iter = 100,main = "scree plot with parallel analysis")
dist <- dist(vertebraldata[,1:6],method = "euclidean",diag = FALSE,upper = FALSE,p=2)
cmd<- cmdscale(dist,k=2)
fa.plot(cmd,title = "Multidimensional Scaling",labels = "")
