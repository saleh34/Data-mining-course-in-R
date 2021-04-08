#14.30
#a).
data = data.frame(
  Time = gl(3,12),
  Operator = gl(4,3,36),
  yield = c(9.5,9.8,10,9.8,10.1,9.6,9.8,10.3,9.7,10,9.7,10.2,10.2,9.9,9.5,10.1,9.8,9.7,10.2,9.8,9.7,10.3,10.1,9.9,10.5,10.2,9.3,10.4,10.2,9.8,9.9,10.3,10.2,10,10.1,9.7)
)
model <- aov(yield ~ Time * Operator ,data=data)
summary(model)
#b)
#s2a = s1^2-s3^2/bn =0.09528 - 0.03269/4*3 = 0.005216
#s2ß = S2^2-S3^2/an =0.02769 - 0.03269/3*3 = -0.00056
#f0.05(2,6) = 5.14 ,f0.05(3,6) = 4.76
#fA = 2.914, fB = 0.8479, fA<5.14 , fB<4.76
#c).
#Though variances are always positive, it is possible to have a situation where the unbiased estimate of the variance is negative. Negative estimates can occur in experiments when an effect is very weak or when there are very few levels corresponding to a variance component.
#The yield does not appear to depend on operator or time.

#14.31
#a). It is called a mixed model
#b).
data1 = data.frame(
Material = gl(3,6),
Brand = gl(3,2,18),
years = c(5.5,5.15,4.75,4.6,5.1,5.2,5.6,5.55,5.5,5.6,5.4,5.5,5.4,5.48,5.05,4.95,4.5,4.55)
)
model1 <- aov(years ~ Material * Brand ,data=data1)
summary(model1)
#b) 
#H0 : Ba=Bb=Bb=0
#H1 : At least one of the Bj is not zero
#f0.05(2,9) = 4.26 , f0.05(2,4) = 6.944 f0.05(4,9)=3.63
# fmaterial = 0.5174/.0109 = 47.42 fBrand = .3033/.1753 = 1.73 fmaterial* fbrand = 16.06
# Here effect of material is significant as f material > 4.26
# The main effect of Brand is not significant as fbrand <6.944
#Hence there is not sufficient evidence to support the claim of brand A manufacturer

#10.80
#The hypotheses are
#H0 : Distribution of grades is uniform,
#H1 : Distribution of grades is not uniform.
Grade <- c(14,18,32,20,16)
res <- chisq.test(Grade, p = c(1/5, 1/5, 1/5,1/5,1/5))
res
# p value is 0.04043 which is less than 0.05
#So we Reject H0.

#10.83
#The hypotheses are
#H0 : f(x) = g(x; 1/2) for x = 1, 2, . . . ,
#H1 : f(x) 6= g(x; 1/2).
x <- c(136,60,34,12,9,5)
res1 <- chisq.test(x, p = c(1/2, 1/4, 1/8,1/16,1/32,1/32))
res1
#we combine the last three classes as less than 5 observations
#Critical region: ??2 > 11.070 with 5 degrees of freedom.
# Therefore we Fail to reject H0

#10.86
#The hypotheses are
#H0 : Presence or absence of hypertension is independent of smoking habits,
#H1 : Presence or absence of hypertension is not independent of smoking habits.
#a = 0.05.
#Critical region: ??2 > 5.991 with 2 degrees of freedom.
Individuals <- matrix(c(21,36,30,48,26,19),ncol=3,byrow=TRUE)
colnames(Individuals) <- c("NonSmokers","Moderatesmokers","Heavysmokers")
rownames(Individuals) <- c("Hypertension","Nohypertension")
tbl <- as.table(Individuals)
tbl
res2 <-chisq.test(tbl)
res2
#As ??2 = 14.464 > 5.991 ,we Reject H0.

#10.89
#The hypotheses are
#H0 : Occurrence of types of crime is independent of city district,
#H1 : Occurrence of types of crime is dependent upon city district.
#Critical region: ??2 > 21.666 with 9 degrees of freedom.
Crimes <- matrix(c(162,118,451,18,310,196,996,25,258,193,458,10,280,175,390,19),ncol=4,byrow=TRUE)
colnames(Crimes) <- c("Assault","Burglary","Larceny","Homicide")
rownames(Crimes) <- c("1","2","3","4")
tbl2 <- as.table(Crimes)
tbl2
res2 <-chisq.test(tbl2)
res2
# ??2 is 124.53 > 21.66 so We Reject H0, occurrence of types of crime is dependent upon city district.
