#AVONA
data("chickwts")
summary(chickwts)
head(chickwts)
meat=chickwts[chickwts$feed=="meatmeal",1]
horse=chickwts[chickwts$feed=="horsebean",1]
result = aov(weight ~ feed, data = chickwts)
summary(result)
table(chickwts)
#             Df Sum Sq Mean Sq F value   Pr(>F)  
#feed         5 231129   46226   15.37 5.94e-10 ***
# P value less than our alpha level(0.05 or 0.01) neans we reject the null hypothesis of equal means for five feed groups.
# which means is different? 
# use Tukey's honest Significance Test (HSD)
TukeyHSD(result, conf.level=0.95)

# ANOVA only works on catogorical variables.It  might missing a contineous factor - might co-variant 
# We use ANCOCA to address this problem (Analysis of covariance)
months= c(78,93,86,57,45,60,28,31,22,9,12,4)
treat =gl(4,3)
lm.mod = lm(months ~ treat)
anova(lm.mod)
#with df is 3, low P value - confirm that treatment level are siginificant
# it didn't consider the duration of treatment.  how long the patience has the cancel. The stage of the desease. 
# The stage of disease is the contributing factor toward survival time
# ? if recommending any treatment at all( or which to study more), we might prefer Treatment 4..
years <- c(1,1,1,2,2,2,3,3,3,4,4,4)
plot(c(1,1,1,2,2,2,3,3,3,4,4,4), months,
     col=c(2,2,2,3,3,3,4,4,4,5,5,5), pch =20,
     xlab="Treament", ylab="Month", main = "Sruvival Time Post Treament")

# another model
lm.mod2 = lm(months ~ years + treat)
anova(lm.mod2)
# years in the covariance
set.seed(1234)
months2 =c(sample(c(78,93,86,57,45,60,28,31,22,9,12,4), 12, replace = F))
years2 =c(sample(c(2.3,3.4,1.8,5.8,6.2,7.3,
                   9.6,11.0,12.2,14.8,17.3,16), 12, replace = F))
treat=gl(4,3)
plot(c(1,1,1,2,2,2,3,3,3,4,4,4), years2,
     col=c(2,2,2,3,3,3,4,4,4,5,5,5), pch =20,cex =2,
     xlab="Treament", ylab="years", main = "Survival Time Post Treament")
# MANOVA
library(MASS)
attach(iris)
boxplot(iris[,"Sepal.Length"]~ Species, 
        data = iris, ylab = "Sepal Length")

boxplot(iris[,"Sepal.Width"]~ Species, 
        data = iris, ylab = "Sepal Width")

boxplot(iris[,"Petal.Length"]~ Species, 
        data = iris, ylab = "Petal Length")

boxplot(iris[,"Petal.Width"]~ Species, 
        data = iris, ylab = "Petal Width")
man.mod = manova(cbind(Sepal.Length, Sepal.Width) ~ Species, data= iris)
man.mod
summary(man.mod)
# Pillai is the default
# more detail with the following
summary.aov(man.mod)
