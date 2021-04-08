#13.2
#Ho : U1=U2=U3=U4=U5
#H1 : At least two means are not equal
hours <- data.frame( c(5.2,4.7,8.1,6.2,3.0,9.1,7.1,8.2,6.0,9.1,3.2,5.8,2.2,3.1,7.2,2.4,3.4,4.1,1.0,4.0,7.1,6.6,9.3,4.2,7.6))
Brands <- data.frame(c(rep("A",5), rep("B",5), rep("C",5), rep("D",5), rep("E",5)))
hours <- unlist(hours)
Brands <- unlist(Brands)
Headache <- data.frame(hours,Brands)
results <- aov(hours ~ Brands, data=Headache)
summary(results)
#Critical region f>2.87 with 4 & 20 df. As F value = 6.587 and P value = 0.0015, so we reject the null hypothesis
#13.6
#Ho : U1=U2=U3
#H1 : At least two means are not equal
sorption <- data.frame(c(1.06,0.79,0.82,0.89,1.05,0.95,0.65,1.15,1.12,1.58,1.45,0.57,1.16,1.12,0.91,0.83,0.43,0.29,0.06,0.44,0.55,0.61,0.43,0.51,0.10,0.53,0.34,0.06,0.09,0.17,0.17,0.60))
Solvents <- data.frame(c(rep("Aromatics",9), rep("Chloroalkanes",8), rep("Esters",15)))
sorption <- unlist(sorption)
Solvents <- unlist(Solvents)
data <- data.frame(sorption,Solvents)
results <- aov(sorption ~ Solvents, data = data)
summary(results)
Aromatics <- c(1.06,0.79,0.82,0.89,1.05,0.95,0.65,1.15,1.12)
mean(Aromatics)
Chloroalkenes <- c(1.58,1.45,0.57,1.16,1.12,0.91,0.83,0.43)
mean(Chloroalkenes)
Esters <- c(0.29,0.06,0.44,0.55,0.61,0.43,0.51,0.10,0.53,0.34,0.06,0.09,0.17,0.17,0.60)
mean(Esters)
#with p value < 0.0001 We reject the null hypothesis
#The mean adsorption rate for chloroalkenes is the highest so it should be used
