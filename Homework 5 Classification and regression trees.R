library(openxlsx)
flightd <- read.xlsx("FlightDelays.xlsx")
DW <- as.factor(flightd$DAY_WEEK)
dummy_DW <- model.matrix(~ DW - 1)
dataframe_dummy_DW <- as.data.frame(dummy_DW)
Carrier <- as.factor(flightd$CARRIER)
dummy_carrier <- model.matrix(~ Carrier - 1)
dataframe_dummy_carrier <- as.data.frame(dummy_carrier)
DA <- as.factor(flightd$ORIGIN)
dummy_DA <- model.matrix(~ DA - 1)
dataframe_dummy_DA <- as.data.frame(dummy_DA)
AA <- as.factor(flightd$DEST)
dummy_AA <- model.matrix(~ AA - 1)
dataframe_dummy_AA <- as.data.frame(dummy_AA)
flightd1 <- cbind(dataframe_dummy_AA[-3],dataframe_dummy_carrier[-8],dataframe_dummy_DA[-3],dataframe_dummy_DW[-7],flightd[,c(1,3,5:7,9,11:15)])
breaks <- c(600,800,1000,1200,1400,1600,1800,2000,2200)
labels <- c("6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22")
bins <- cut(flightd1$CRS_DEP_TIME, breaks, include.lowest = T, right=FALSE, labels=labels)
summary(bins)
dataframebins <- as.data.frame(bins)
bins1 <- as.factor(bins)
dummy_bins <- model.matrix(~ bins1 - 1)
dataframe_dummy_bins <- as.data.frame(dummy_bins)
flightd2 <- cbind(dataframe_dummy_bins[-8],flightd1[,c(-19)])
table(flightd2$Flight.Status)
set.seed(123)
train_sample1 <- sample(2201,1321)
flightd2_train <- flightd2[train_sample1,]
flightd2_test <- flightd2[-train_sample1,]
prop.table(table(flightd2_train$Flight.Status))
prop.table(table(flightd2_test$Flight.Status))
library(C50)
library(gmodels)
flightd2_train$Flight.Status <- as.factor(flightd2_train$Flight.Status)
flightd2_model <- C5.0(flightd2_train[-32],flightd2_train$Flight.Status)
summary(flightd2_model)
library(rpart)
library(rpart.plot)
ctrl = rpart.control(maxdepth=6)
flightd2model1 <- rpart(Flight.Status ~ . , data=flightd2_train,method="class",control = ctrl, minsplit = 20 ,cp=-1)
summary(flightd2model1)
varImp(flightd2model1)
path.rpart(flightd2model1, nodes=101, pretty=0, print.it = TRUE)
rpart.plot(flightd2model1,tweak=3)
bestcpd <- flightd2model1$cptable[which.min(flightd2model1$cptable[,"xerror"]),"CP"]
plotcp(flightd2model1)
printcp(flightd2model1)
flightd2model1best <-prune(flightd2model1,cp=.307392996108949)
summary(flightd2model1best)
rpart.plot(flightd2model1best,tweak=2)
path.rpart(flightd2model1best,nodes=3,pretty=0,print.it = TRUE)
library(rpart.plot)
flightd3 <- cbind(dataframe_dummy_bins[-8],flightd1[,c(-24)])
flightd3_train <- flightd3[train_sample1,]
flightd3_test <- flightd3[-train_sample1,]
mytree <- rpart(Flight.Status ~ . ,data=flightd3_train, method="class", minsplit = 20, minbucket = 7, cp=-1)
summary(mytree)
rpart.plot(mytree,tweak=4)
bestcp <- mytree$cptable[which.min(mytree$cptable[,"xerror"]),"CP"]
printcp(mytree)
treeOptimal <- prune(mytree,cp=mytree$cptable[which.min(mytree$cptable[,4]),1])
summary(treeOptimal)
rpart.plot(treeOptimal,tweak=2)
library(caret)
varImp(mytree)
library(openxlsx)
Toycor <- read.xlsx("ToyotaCorolla.xlsx",sheet=2)
FT <- as.factor(Toycor$Fuel_Type)
dummy_FT <- model.matrix(~ FT - 1)
dataframe_dummy_FT <- as.data.frame(dummy_FT)
Color <- as.factor(Toycor$Color)
dummy_color <- model.matrix(~ Color - 1)
dataframe_dummy_color <- as.data.frame(dummy_color)
Toycor <- Toycor[-1]
Toycor1 <- cbind(dataframe_dummy_FT[-3],dataframe_dummy_color[-10],Toycor[,c(-7,-10)])
spec = c(train = .5, test = .2, validate = .3)
g = sample(cut(
  seq(nrow(Toycor1)), 
  nrow(Toycor1)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(Toycor1, g)
sapply(res, nrow)/nrow(Toycor1)
addmargins(prop.table(table(g)))
set.seed(1)
Toycor2 <- res$train
Toycor3 <- Toycor2[,c(1,2,13,14,17,18,20,22,25,27,29,33,34,36,38,42,47)]
library(rpart)
Toycor3model1 <- rpart(Price ~ . , data= Toycor3,method = "anova",maxdepth=6 ,minbucket=1,cp=-1)
summary(Toycor3model1)
library(caret)
varImp(Toycor3model1)
Toycor3_pred <- predict(Toycor3model1, Toycor3)
Toycor3_pred
Toycor3_pred1 <- as.data.frame(Toycor3_pred)
library(Metrics)
rmsetrain <- rmse(Toycor3$Price,Toycor3_pred)
Toycor4 <- res$validate
Toycor5 <- res$test
Toycor41 <- Toycor4[,c(1,2,13,14,17,18,20,22,25,27,29,33,34,36,38,42,47)]
Toycor51 <- Toycor5[,c(1,2,13,14,17,18,20,22,25,27,29,33,34,36,38,42,47)]
Toycor4_pred <-predict(Toycor3model1,Toycor41)
Toycor4_pred<- as.data.frame(Toycor4_pred)
Toycor5_pred <-predict(Toycor3model1,Toycor51)
Toycor5_pred<-as.data.frame(Toycor5_pred)
rmsevalidate <- rmse(Toycor4$Price,Toycor4_pred)
rmsetest <- rmse(Toycor5$Price,Toycor5_pred)
rmsetest
rmsetrain
rmsevalidate
petrain <- (Toycor3$Price-Toycor3_pred1)
peval <- (Toycor41$Price-Toycor4_pred)
petest <- (Toycor51$Price-Toycor5_pred)
par(mfrow=c(1,3))
boxplot(petrain,main="boxplot of training set",ylab="Prediction Error")
boxplot(peval,main="boxplot of validation set",ylab="Prediction Error")
boxplot(petest,main="boxplot of test set",ylab="Prediction Error")
breaks1 <- c(3500,5000,6500,8000,9500,11000,12500,14000,15500,17000,18500,20000,21500,23000,24500,26000,27500,29000,30500,32000,33500)
labels1 <- c("3500-5000","5000-6500","6500-8000","8000-9500","9500-11000","11000-12500","12500-14000","14000-15500","15500-17000","17000-18500","18500-20000","20000-21500","21500-23000","23000-24500","24500-26000","26000-27500","27500-29000","29000-30500","30500-32000","32000-33500")
bins1 <- cut(Toycor1$Price, breaks1, include.lowest = T, right=FALSE, labels=labels1)
summary(bins1)
dataframebins1 <- as.data.frame(bins1)
Toycorb <- cbind(dataframebins1,Toycor1[,c(-13)])
spec1 = c(train = .5, test = .2, validate = .3)
g1 = sample(cut(
  seq(nrow(Toycorb)), 
  nrow(Toycorb)*cumsum(c(0,spec1)),
  labels = names(spec1)
))

res1 = split(Toycorb, g1)
sapply(res, nrow)/nrow(Toycor1)
Toycorb2 <- res1$train
Toycorb3 <- Toycorb2[,c(1,2,3,14,17,18,20,22,25,27,29,33,34,36,38,42,47)]
library(rpart)
Toycorb3model1 <- rpart(bins1 ~ . , data= Toycorb3,method = "class",maxdepth=6 ,minbucket=3,cp=-1)
summary(Toycorb3model1)
Toycorb3model1p <-predict(Toycorb3model1,Toycorb3,type="class")
Toycorb3model1p
dt <- as.data.frame(Toycorb3model1p)
Newrow <- c(0,0,0,77,117000,110,0,5,100,0,3,1,0,0,0,0,1)
Toycorb31 <- rbind(Toycorb3,Newrow)
Toycorb31a <- Toycorb31[719:719,]
Toycor3bar_pred <- predict(Toycor3model1, Toycorb31a)
Toycor3bar_pred
Toycor3bar_pred1 <- as.data.frame(Toycor3bar_pred)
Toycor3bac_pred1 <- predict(Toycorb3model1, Toycorb31a,type="class")
Toycor3bac_pred1
