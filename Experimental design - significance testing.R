#14.2
# a) H0 : a1 = a2 = a3 = 0,
#H1 : At least one of the aiís is not zero;
#(b)H'0 : ﬂ1 = ﬂ2 = ﬂ3 = 0,
#H'1 :At least one of the ﬂiís is not zero ;
#(c) H''0 : (aﬂ)11 = (aﬂ)12 = ∑ ∑ ∑ = (aﬂ)33 = 0,
#H''1 : At least one of the (aﬂ)ij ís is not zero.
aapl = data.frame(
  Brand = gl(3,12),
  Time = gl(3,4,36),
  ascorbicacid = c(52.6,54.2,49.8,46.5,49.4,49.2,42.8,53.2,42.7,48.8,40.4,47.6,56.0,48.0,49.6,48.4,48.8,44,44,42.4,49.2,44,42,43.2,52.5,51.8,52,53.6,48,47,48.2,49.6,48.5,43.3,45.2,47.6)
)
model <- aov(ascorbicacid ~ Brand * Time ,data=aapl)
summary(model)
#(a) Do not reject H0 as pvalue >0.05, pvalue = 0.195331;
#(b) Reject H'0as p value <0.05, p value = 0.000183 ;
#(c) Do not reject H''0 as p value >0.05, pvalue = 0.765025 .

#14.4
#H0 : a1 = a2 = a3 = 0
#H1 : At least one of the aiís is not zero;
#H'0 : ﬂ1 = ﬂ2 = ﬂ3 = 0,
#H'1 : At least one of the ﬂiís is not zero;
#H''0 : (aﬂ)11 = (aﬂ)12 = ∑ ∑ ∑ = (aﬂ)33 = 0,
#H''1 : At least one of the (aﬂ)ij ís is not zero.
data1 = data.frame(
  Coating = c(rep("Uncoated", 18), rep("Anodized", 18),
              rep("Conversion", 18)),
  Relative_Humidity = c(rep(c("Low", "Medium", "High"), 18)),
  Corrosionfatigue = c(361,314,1344,469,522,1216,466,244,1027,937,739,1097,1069,261,1011,1357,134,1011,114,322,78,1032,471,466,1236,306,387,92,130,107,533,68,130,211,398,327,130,252,586,1482,874,524,841,105,402,529,755,751,1595,847,846,754,573,529)
)
model1 <- aov(Corrosionfatigue ~ Coating * Relative_Humidity ,data=data1)
summary(model1)
# Since p value less than 0.05, p value = 0.00144 so we reject H0
# Since p value less than 0.05, p value = 0.01188 so we reject H'0
# Since p value less than 0.05, p value = 0.02819 so we reject H''0
library(agricolae)
duncan.test(model1,"Relative_Humidity",console = TRUE)
library(DescTools)
PostHocTest(model1, method = "duncan")
#Humidity level medium results in significantly different corrosion fatigue damage compared to other two levels. The same can be seen from different letters in the duncan test and the pvalue being less than 0.05(pvalue of 0.0264 and 0.0062 in medium-high and medium-low) in the duncan test 
#14.14
#a)
data2 = data.frame(
  Copper = c(rep(1, 9), rep(2, 9),
              rep(3, 9)),
  Time = c(rep(c(5, 12, 18), 9)),
  Algae = c(0.30,0.37,.25,.34,.36,.23,.32,.35,.24,.24,.30,.27,.23,.32,.25,.22,.31,.25,.20,.30,.27,.28,.31,.29,.24,.30,.25)
)
model3 <- lm(Algae ~ Copper*Time, data=data2)
summary(model3)
#The fitted model would beày = 0.41772 - 0.06631x1 - 0.00866x2 + 0.00416x1x2,with the P-values of the t-tests on each of the coefficients as 0.0092, 0.0379 and 0.0318 for x1, x2, and x1x2, respectively. They are all significant at a level larger
#than 0.0379. Furthermore, R2adj = 0.1788.
#b)
model4 <- lm(Algae ~ Copper*Time+ I(Copper^2) + I(Time^2), data=data2)
summary(model4)
#The new fitted model is ày = 0.3368 - 0.15965x1 + 0.02684x2 + 0.00416x1x2 + 0.02333x21- 0.00155x22
#,with P-values of the t-tests on each of the coefficients as 0.0004, 4.63e-05, 0.0003,
#0.0156, and 3.05e-07 for x1, x2. x1x2, x21 and x22 respectively. 
#Furthermore,R2adj = 0.7700 which is much higher than that of the model in (a). Model in (b) would be more appropriate.

#14.16
data4 = data.frame(
  
  A = gl(4,12),
  B = gl(2,3,48),
  C = gl(3,1,48),
  Y = c(4.0,3.4,3.9,4.4,3.1,3.1,4.9,4.1,4.3,3.4,3.5,3.7,3.6,2.8,3.1,2.7,2.9,3.7,3.9,3.2,3.5,3.0,3.2,4.2,4.8,3.3,3.6,3.6,2.9,2.9,3.7,3.8,4.2,3.8,3.3,3.5,3.6,3.2,3.2,2.2,2.9,3.6,3.9,2.8,3.4,3.5,3.2,4.3)
)
results5 <- aov(Y ~ A + B*C, data=data4)
summary(results5)
results6 <- aov(Y ~ A + B+ C, data=data4)
summary(results6)
#When only A, B, C, and BC factors are in the model, the P-value for BC interaction is 0.0806. Hence at level of 0.05, the interaction is insignificant.
#The P-values of the main effects of A, B, and C are 0.0275, 0.0224, and 0.0131, respectively. All
#these are significant.

#14.17
data5 = data.frame ( A= gl(2,27),B= gl(3,1,54), C= gl(3,3,54), Y= c(15,14.8,15.9,16.8,14.2,13.2,15.8,15.5,19.2,18.5,13.6,14.8,15.4,12.9,11.6,14.3,13.7,13.5,22.1,12.2,13.6,14.3,13,10.1,13,12.6,11.1,11.3,17.2,16.1,18.9,15.4,12.4,12.7,17.3,7.8,14.6,15.5,14.7,17.3,17,13.6,14.2,15.8,11.5,18.2,14.2,13.4,16.1,18.6,15.2,15.9,14.6,12.2))
result6 <- aov(Y ~ A*B*C, data=data5)
summary(result6)
#Based on the P-values, only AB and AC interactions are significant as p values are 0.0311 and 0.0319
#The main effect B is significant as p = 0.00302. However, due to significant interactions mentioned in (a), the insignificance of A and C cannot be counted.
op <- par(mfrow = c(1, 1))
with(data5, {
  interaction.plot(C,A,Y)
}
)
#the mean responses at different levels of C varies in different patterns for the different levels of A. Hence, although the overall test on factor C is insignificant, it is misleading since the significance of the effect C is masked by the significant interaction between A and C.

#14.36
data7 = data.frame(
  Environment = c(rep("Dry hydrogen", 9), rep("High humidity", 9)),
  stress = c(rep(c("low", "medium", "high"), 6)),
   fatiguelife= c(11.08,13.12,14.18,10.98,13.04,14.9,11.24,13.37,15.1,10.75,12.73,14.15,10.52,12.87,14.42,10.43,12.95,14.25)
)
model9 <- aov(fatiguelife ~ Environment*stress, data=data7)
summary(model9)
#The interaction is insignificant as p value is 0.75
#(b) The mean fatigue life for the two main effects are all significant as p values for environment and stress are 0.00209 & 1.9e-11. 