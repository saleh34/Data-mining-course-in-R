library(openxlsx)
Toycor <- read.xlsx("18 Toyota Corolla(1).xlsx")
summary(Toycor)
Fuel <- as.factor(Toycor$Fuel_Type)
dummy_fuel <- model.matrix(~ Fuel - 1)
dataframe_dummy_fuel <- as.data.frame(dummy_fuel)
Toycor1 <- cbind(dataframe_dummy_fuel[-3],Toycor[,c(3:4,7,9,12,14,17,19,21,25,26,28,30,34,38)])
train_sample <- sample(1436,1077)
Toycor1_train <- Toycor1[train_sample,]
Toycor1_test <- Toycor1[-train_sample,]
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
Toycor1_norm <- as.data.frame(lapply(Toycor1,normalize))
Train_ <- Toycor1_norm[train_sample,]
Test_ <- Toycor1_norm[-train_sample,]
library(neuralnet)
n <- names(Toycor1_train)
f <- as.formula(paste("Price ~", paste(n[!n %in% "Price"], collapse = " + ")))
f
Toycor_model <- neuralnet(f, data = Train_ , hidden = 5,algorithm ='backprop', stepmax=1000000, learningrate = 0.1,linear.output = F, threshold = 0.000001,rep = 1)
plot(Toycor_model)
model_results <- compute(Toycor_model,Train_[,c(1:2,4:17)])
predicted_price <- model_results$net.result
pp <- as.data.frame(predicted_price)
model_resultst <- compute(Toycor_model,Test_[,c(1:2,4:17)])
predicted_pricet <- model_resultst$net.result
ppt <- as.data.frame(predicted_pricet)
minvec <- sapply(Toycor1_train,min)
maxvec <- sapply(Toycor1_train,max)
traindenorm <- mapply(function(x, y) (x*(max(y)-min(y)))+min(y), pp$V1, Toycor1_train$Price)
Toycor1_denorm <- as.data.frame(lapply(pp,denormalize))
