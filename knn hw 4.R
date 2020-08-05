library(readxl)
BH <- read_xlsx("BostonHousing.xlsx",sheet=2)
normalized <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

BH_N <-as.data.frame(lapply(BH,normalized))
BH_train <- BH_N[1:304,]
BH_validation <- BH_N[305:506,]
BHS <- read_xlsx("BostonHousing.xlsx",sheet=2)
BHTL <- BHS[1:304,]
BHVL <- BHS[305:506,]
BH_train_labels <- BHTL$`CAT. MEDV`
BH_validation_labels <- BHVL$`CAT. MEDV`

library(class)
BH_validation_pred1 <- knn(train = BH_train[,1:13], test = BH_validation[,1:13], cl=BH_train_labels, k=1)
library(gmodels)
CrossTable(x=BH_validation_labels, y=BH_validation_pred1, prop.chisq = FALSE)
#Error = 3

BH_Validation_pred2 <- knn(train = BH_train[,1:13], test= BH_validation[,1:13], cl=BH_train_labels,k=2)
CrossTable(x=BH_validation_labels, y=BH_Validation_pred2, prop.chisq = FALSE)
#Error = 3
BH_Validation_pred3 <- knn(train = BH_train[,1:13], test= BH_validation[,1:13], cl=BH_train_labels,k=3)
CrossTable(x=BH_validation_labels, y=BH_Validation_pred3, prop.chisq = FALSE)
#Error = 2
BH_Validation_pred4 <- knn(train = BH_train[,1:13], test= BH_validation[,1:13], cl=BH_train_labels,k=4)
CrossTable(x=BH_validation_labels, y=BH_Validation_pred4, prop.chisq = FALSE)
#Error = 4
BH_Validation_pred5 <- knn(train = BH_train[,1:13], test= BH_validation[,1:13], cl=BH_train_labels,k=5)
CrossTable(x=BH_validation_labels, y=BH_Validation_pred5, prop.chisq = FALSE)
#Error = 7

#Best K = 3





BHnewrow <- c(0.2,0,7,0,0.538,6,62,4.7,4,307,21,10)
BHaddon <- rbind(BH,BHnewrow)
BH_N_ADDON <-as.data.frame(lapply(BHaddon[,1:13],normalized))
BH_ADDON <- BH_N_ADDON[507:507,]
BH_ADDONL <- BHaddon[507:507,]
BH_ADDON_train_labels <- BHTL$`MEDV`
BH_ADDON_lable <- BH_ADDONL$`MEDV`
BH_ADDON_validation_pred <- knn(train = BH_train[,1:12], test = BH_ADDON[,-13], cl=BH_ADDON_train_labels, k=3)
BH_ADDON_validation_pred
BH_ADDON_TRAINING_PREDICT <- knn(train = BH_train[,1:13], test = BH_train[,1:13], cl=BH_train_labels)
CrossTable(x=BH_validation_labels, y=BH_ADDON_TRAINING_PREDICT, prop.chisq = FALSE)
