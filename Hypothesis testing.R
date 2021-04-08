#13.7
#H0: U1=U2=U3=U4
#H1: At least two means are not equal
heightchange <- data.frame(c(13.2,12.8,13.0,14.2,15.0,12.4,17.2,14.0,21.6,20.0,16.0,14.8,14.0,14.0,22.2,12.6,13.0,23.6,17.0,24.4,7.8,20.0,17.0,19.6,20.2,14.4,15.8,27.0,18.0,23.2,21.0,19.1,18.0,21.1,25.0,14.8,15.8,26.0,22.0,18.2))
concentration <- data.frame(c(rep("Fifty",10),rep("Hundred",10),rep("Twohundred",10),rep("Fourhundred",10)))
heightchange <- unlist(heightchange)
concentration <- unlist(concentration)
data <- data.frame(heightchange,concentration)
results <- aov(heightchange ~ concentration, data=data)
summary(results)
#As p value = 0.0989 we fail to reject null hypothesis
#There is no evidence to support that different concentrations of MgNH4PO4 affect the average attained height of chrysanthemums.
g_1 <- c(13.2,12.8,13.0,14.2,15.0,12.4,17.2,14.0,21.6,20.0)
g_2 <- c(16.0,14.8,14.0,14.0,22.2,12.6,13.0,23.6,17.0,24.4)
g_3 <- c(7.8,20.0,17.0,19.6,20.2,14.4,15.8,27.0,18.0,23.2)
g_4 <- c(21.0,19.1,18.0,21.1,25.0,14.8,15.8,26.0,22.0,18.2)
data <- data.frame(activity_level=c(g_1, g_2, g_3, g_4), drug_group=c(rep("g_1", times = length(g_1)), rep("g_2", times = length(g_2)), rep("g_3", times = length(g_3)), rep("g_4", times = length(g_4))))
#13.8
#H0: Variances are equal
#H1: At least two variances are significantly different
bartlett.test(activity_level ~ drug_group, data = data)
#p-value is 0.5066 so we fail to reject the null hypothesis and hence the variances can be assumed to be equal

#13.13
#H0: U1=U2=U3=U4
#H1: At least two of the means are not equal
lengths <- data.frame(c(13.7,23.0,15.7,25.5,15.8,14.8,14.0,29.4,9.7,14.0,12.3,12.3,6.2,5.4,5.0,4.4,5.0,3.3,16.0,2.5,1.6,3.9,2.5,7.1,27.2,16.8,12.9,14.9,17.1,13.0,10.8,13.5,25.5,14.2,27.4,11.5,18.2,8.8,14.5,14.7,17.1,13.9,10.6,5.8,7.3,17.7,18.3,9.9))
baths <- data.frame(c(rep("BathsI-V",12),rep("BathsII-V",12),rep("BathsI-X",12),rep("BathsII-X",12)))
lengths <- unlist(lengths)
baths <- unlist(baths)
data1 <- data.frame(lengths,baths)
results <- aov(lengths ~ baths, data=data1)
summary(results)
pairwise.t.test(data1$lengths, data1$baths, p.adjust.method = "bonf",alpha=0.01)
#Bath I and Bath II are significantly different for 5 launderings as p value of 1.4 e-5 is less than 0.01.
#Bath I and Bath II are not significantly different for 10 launderings as p value of 0.3890 is more than 0.01.  

#13.16
#H0: U1=U2=U3=U4
#H1: At least two of the means are not equal
reductions <- data.frame(c(25.6,24.3,27.9,25.2,28.6,24.7,20.8,26.7,22.2,31.6,29.8,34.3))
blends <- data.frame(c(rep("One",3),rep("Two",3),rep("Three",3),rep("Four",3)))
reductions <- unlist(reductions)
blends <- unlist(blends)
data2 <- data.frame(reductions,blends)
results <- aov(reductions ~ blends, data=data2)
summary(results)
library(agricolae)
duncan.test(results,"blends",console = TRUE)
library(DescTools)
PostHocTest(results, method = "duncan")
#Blend 4 is significantly different from other blends as can be seen from the different letter of blend 4 from other 3 groups and p value less than 0.05 in different comparisons.
TukeyHSD(results, "blends")
plot(TukeyHSD(results, "blends"))
#Blend 4 is significantly different from Blend 3 as can be seen from the plot and the p value being less than 0.05
#13.39
#H0 : Sigma^2(alpha) = 0
#H1 : Sigma^2(alpha) not equal to 0
output <- data.frame(c(175.4,171.7,173,170.5,168.5,162.7,165,164.1,170.1,173.4,175.7,170.7,175.2,175.7,180.1,183.7))
operators <- data.frame(c(rep("one",4),rep("two",4),rep("three",4),rep("four",4)))
output <- unlist(output)
operators <- unlist(operators)
data3 <- data.frame(output,operators)
results2 <- aov(output ~ (operators), data=data3)
summary(results2)
#pvalue is 0.0002 thus we reject null hypothesis
#Sigma^2(alpha)= S1^2-S^2/n = 123.96-8.32/4 = 28.9

#13.45
#a)
#H0 : Sigma^2a = Sigma^2b = Sigma^2c = Sigma^2d
#H1 : The variances are not all equal
results <- data.frame(results=c(58.7,61.4,60.9,59.1,58.2,62.7,64.5,63.1,59.2,60.3,55.9,56.1,57.3,55.2,58.1,60.7,60.3,60.9,61.4,62.3))
laboratory <- data.frame(laboratory=(c(rep("A",5),rep("B",5),rep("C",5),rep("D",5))))
results <- unlist(results)
laboratory <- unlist(laboratory)
data4 <- data.frame(results,laboratory)
bartlett.test(results ~ laboratory, data = data4)
#As p value is 0.2806 there is not sufficient evidence to prove variances are not equal
#b
#H0 : Ua=Ub=Uc=Ud
#H1 : At least two means are not equal 
results5 <- aov(results ~ laboratory, data=data4)
summary(results5)
#As p value is 0.000128 reject the null hypothesis
#c)
library(gplots)
plot(results5, 2)
