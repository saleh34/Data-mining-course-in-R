#12.5
data <- data.frame(  y=c(240,236,290,274,301,316,300,296,267,276,288,261),
                     x1=c(25,31,45,60,65,72,80,84,75,60,50,38),
                     x2=c(24,21,24,25,25,26,25,25,24,25,25,23),
                     x3=c(91,90,88,87,91,94,87,86,88,91,90,89),
                     x4=c(100,95,110,88,94,99,97,96,110,105,100,98))
library(car)
lm_1 <- lm(y ~ ., data=data)
summary(lm_1)
#12.8
data2 <- data.frame( y=c(25.2,27.3,28.7,29.8,31.1,27.8,31.2,32.6,29.7,31.7,30.1,32.3,29.4,30.8,32.8),
                     x=c(10,10,10,15,15,15,20,20,20,25,25,25,30,30,30))
lm_2 <- lm(y ~ x + I(x^2), data=data2)
summary(lm_2)
library(EnvStats)
library(alr3)
pureErrorAnova(lm_2)
#12.19
anova(lm_1)
#see mean sq in residuals
#12.26
newdata <- data.frame(x=19.5)
predict(lm_2, newdata, interval="confidence",level=0.90)
#12.29
data3 <- data.frame( y=c(193,230,172,91,113,125),
                     x1=c(1.6,15.5,22,43,33,40),
                     x2=c(851,816,1058,1201,1357,1115))
lm_3 <- lm(y ~ ., data=data3)
summary(lm_3)
#a)We fail to reject H0 as p value = 0.3562
#b)We fail to reject H0 as p value = 0.1841
#c)There is no sufficfient evidence that x1 and x2 significantly influence the response of the model.Hence we need to modify the model
#12.33
data <- data.frame(  y=c(240,236,290,274,301,316,300,296,267,276,288,261),
                     x1=c(25,31,45,60,65,72,80,84,75,60,50,38),
                     x2=c(24,21,24,25,25,26,25,25,24,25,25,23),
                     x3=c(91,90,88,87,91,94,87,86,88,91,90,89),
                     x4=c(100,95,110,88,94,99,97,96,110,105,100,98))
library(car)
lm_1 <- lm(y ~ ., data=data)
summary(lm_1)
anova(lm_1)
#12.34
lm_4 <- lm(y ~ x3 + x4,data=data)
anova(lm_4,lm_1)
#As p =.0085 we reject the null hypothesis
#12.42a
data7 <- data.frame(  y=c(82,93,114,124,111,129,157,164),
                     x1=c(-1,1,-1,-1,1,1,-1,1),
                     x2=c(-1,-1,1,-1,1,-1,1,1),
                     x3=c(-1,-1,-1,1,-1,1,1,1))
lm_7 <- lm (y ~ .,data=data7)
summary(lm_7)
lm_8 <- lm (y ~ x2 + x3 ,data=data7)
summary(lm_8)
#There appears to be little advantage using the full model based on R2 adj.
#12.46
data9 <- data.frame(  Profit=c(157,-181,-253,158,75,202,-451,146,89,-357,522,78,5,-177,123,251,-56,453,288,-104),
                      Income=c(45000,55000,45800,38000,75000,99750,28000,39000,54350,32500,36750,42500,34250,36750,24500,27500,18000,24500,88750,19750),
                      Gender=c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1),
                      FamilyMember=c(1,2,4,3,4,4,1,2,1,1,1,3,2,3,2,1,1,1,1,2))
lm_9 <- lm(Profit ~ Income + FamilyMember + Gender ,data=data9)
summary(lm_9)
# Here we code gender male as 0 and female as 1. The results show company would prefer femnale customers
#Since pvalue >0.05 it is not significant but close to significance.So income is not a significantly important factor.