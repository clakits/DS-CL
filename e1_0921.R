# Learning statistics: Concepts and Applications in R 
# Lessons learned
# Do NOT overwrite built-in functions - check the variable name with ? to see if 
# Don't do this! mean = (5+7)/2
# Do NOT use "c" or "t" as variable name



#09/21/2018 
x <- c(1,2,3,4,5)
y <- c(1,8,27,64,125)
plot(x,y)
data(faithful)
plot(faithful, main ="Old Faithful Eruptions", xlab = "Eruption Length(min)", 
                       ylab = "Wait time (min)", pch = 20, col = "red")
hist(faithful$waiting, main ="Histogram of Faithful Waiting (min)")
#Breaks
hist(faithful$waiting, plot = FALSE)$breaks
#Counts
hist(faithful$waiting, plot = FALSE)$counts
#change the bin size
hist(faithful$waiting, main = "Histogram",breaks = seq(from=40, to=100, by=1))
# Breaks
hist(faithful$waiting,  
     breaks = seq(from=40, to=100, by=1), plot=FALSE)$breaks
#Counts
hist(faithful$waiting,  
     breaks = seq(from=40, to=100, by=1), plot=FALSE)$counts

#qqplot - check if the data is normal distributed
x = rnorm(300)
qqnorm(x, pch=16, cex = .5)
qqline(x)
x
qqnorm(faithful$waiting,pch = 16, cex=.5, main = "Q-Q Plot for Waiting Time")
qqline(faithful$waiting)

#axiom probability 
#1. for any event A, 0<= P(A) <= 1
#2. If S is the sample space, P(S) =1, and P(0) =0 (not in the sample set)
#3. Union P(A or B) is denoted P(A U B); Intersection P(A and B) is denoted P (A n B)
# If A and B are mutually exclusive (don't overlap), then probability of thier union = sum of their probabilities
# A complement is P(A  c) = 1 - P(A)
# (P(A U B)=P(A) + P(B) - P(A n B))
# P(A n B) = P(A) * P(B)
# Laws of probability... 
#conditional probality P(A | B) = P(A n B)/P(B)
# Bayes' rule P(A|B) = P(B|A)P(A)/P(B) - The probability of A given B is equal to the 
# probability of B given A * P(A)/P(B) 


# a discrete number
#PMF - 
# population mean - 
# variance - measures the spread of a random variable's distribution around its mean
# the standard deviation of a random variable is the square root of its variance. 

# X is binomial with two parameters
# n = total number of flips
# p = probability of success if it consists of a series of independent trials

# X ~ Bin(3, 0.03)

#Normal Distribution - proximaty 
# The shape is symmetric
# The mean, median and mode are all the same
# The bulk of the probability is toward the middle of the distribution
# P6 for notes
curve(dnorm(x,15,7), xlim = c(-20, 30),ylim=c(0, .4), col="red", lwd=3)
curve(dnorm(x,-2,3), xlim = c(-20, 30),ylim=c(0, .4), col="red", lwd=3)
curve(dnorm(x,5,2), xlim = c(-20, 30),ylim=c(0, .4), col="red", lwd=3)
#Stardard Normal Distribution mean is 0 is standard deviation is 1

curve(dnorm(x,0,1), xlim = c(-20, 30),ylim=c(0, .4), col="red", lwd=3)
# Recenter X to have a mean of 0
pnorm(45, mean=50, sd=10)
pnorm(1.8820239, lower.tail = FALSE)
# lower.tail = TRUE returns the probability contained in the lower (left) tail, P (X <= a)
pnorm(60, mean = 50, sd =10, lower.tail = FALSE)
# what about P(45<= X <= 60)?
pnorm(60, mean = 50, sd =10) - pnorm(45, mean=50, sd=10)

pnorm(2) - pnorm(-2)
pnorm(3) - pnorm(-3)
# To find the 90th percentile of x ~ N(10,5), we seek the value a such that P(X <= a) = 0.90
qnorm(0.90, mean =10, sd = 5)
# or use Z, then 1.28 * 5 + 10(Z = x-u/standardDeviation)
qnorm (0.90)
qnorm(0.90)*5 + 10
if the qth percentile of the standard normal distribution is the vlaue a, what is the percentile of the value -a?
qnorm(0.90)
qnorm(0.10)
pnorm(158, mean = 129, sd = 23, lower.tail = FALSE)
qnorm(.95, mean = 129, sd = 23)
qnorm(.95)
qnorm(.95) *23 + 129
# covariance - postive covariance" random variables tent ot move TOGETHER; 
# Negative covariance: random variables move OPPOSITE each other
# if the data is NOT measured in the same unit; we can't compare the covariance.
# Covariance can't tell us how strong the relationsip is between X and Y
# devide to the SD; we need scale to a dimenstionless measure - correlation coeffiency
# Correlation is unitless, so we can directly compare all types of variables
# covariance and corrleation coeffiency are measures of the degree of linear association (Linear relationships and normal distribution)
# Correlation does NOT imply causality

y = c(58,75,71,77,80,88,83,95)
plot(x,y, main="Hours spent studying vs test score", xlab = "hours studying", ylab = "test score", pch=20, col="blue")
abline(h=mean(y), col = 2, lwd = 2)
abline(v=mean(x), col = 4, lwd = 2)
# (2-7)*(58-78.4) + (3-7)*(75-78.4)..../n-1
cov(x,y)
#cov(x,y)/(sd(x)*sd(y)) is the same cor(x,y)
cov(x,y)/(sd(x)*sd(y))
cor(x,y)
# Correlation is unitless, so we can directly compare all types of variables
# Correlation goes form -1 to 1

data(faithful)
round(cor(faithful), 4)
# Q: how to find out the dataset in the packages? 
data("Harman23.cor")
round(Harman23.cor$cov,2)
install.packages("car")
library("car")
data("Salaries")
head(Salaries)


# Visualization
install.packages("RColorBrewer")
library(datasets)
library(RColorBrewer)
attach(iris)

#Bar graph - plot catagory data.. x is not quantitive. 
barplot(iris$Petal.Length, main = "Petal Length")

barplot(iris$Petal.Length, main = "Petal Length", border = "NA")
barplot(iris$Sepal.Length, main = "Sepal Length")
barplot(table(iris$Species,iris$Sepal.Length), col = brewer.pal(3, "Set1"), main = "Stacked Plot of Sepal Length by Species")

barplot(table(iris$Species,iris$Petal.Length), col = brewer.pal(3, "Set2"), main = "Stacked Plot of Petal Length by Species")
head(iris)
summary(iris[, 1:2])
summary(iris[, 3:4])
summary(iris[, 5])

# Box plots show summary
boxplot(iris$Sepal.Length, main = "Petal Length")
boxplot(iris[,1:4], names = c("SL", "SW", "PL", "PW"))
boxplot(iris$Petal.Length~iris$Species, main = "Petal Length vs Species")
boxplot(iris$Petal.Length~iris$Species, col = heat.colors(3), main = "Petal Length vs Species")
boxplot(iris$Petal.Width~iris$Species, main = "Petal Width vs Species")

boxplot(iris$Sepal.Length~iris$Species, main = "Sepal Length vs Species")
boxplot(iris$Sepal.Width~iris$Species, main = "Sepal Width vs Species")


#scalplot - shows the value of petal length
plot(iris$Petal.Length, main = "Petal Length", ylab = "Petal Length", xlab = "Species")

# set the plot side by side
par(mfrow = c(1,2))
plot(iris$Petal.Length, main = "Petal Length", ylab = "Petal Length", xlab = "Species")
plot(iris$Sepal.Length, main = "Sepal Length", ylab = "Sepal Length", xlab = "Species")

par(mfrow = c(1,2))
plot(iris$Petal.Width, main = "Petal Width", ylab = "Petal Width", xlab = "Species")
plot(iris$Sepal.Width, main = "Sepal Width", ylab = "Sepal Width", xlab = "Species")


# histogram
hist(iris$Petal.Width, breaks =13)
hist(iris$Petal.Width, breaks =25)
b = seq(min(iris$Petal.Width), max(iris$Petal.Width), length = 11)
hist(iris$Petal.Width, breaks =b, xlab="Petal Width", main = "Histogram of Petal Width")
# histogram pitfalls - break size matters - not enough data if they are normal distributed. One way to 
# overcome is to add density line to histogram 
#hist(iris$Petal.Width, breaks =3) # prob = TRUE
#lines(density(iris$Petal.Width))


hist(iris$Petal.Width, prob=TRUE, breaks =3)
lines(density(iris$Petal.Width))


hist(iris$Petal.Width, prob=TRUE, breaks =5)
lines(density(iris$Petal.Width))
# density 
par(mfrow = c(1,4))
dens.pw = density(iris$Petal.Width)
plot(dens.pw, ylab = "Frequency", xlab = "Width", main = "Petal Width Density")

dens.pl = density(iris$Petal.Length)
plot(dens.pl, ylab = "Frequency", xlab = "Length", main = "Petal Length Density")


dens.sw = density(iris$Sepal.Width)
plot(dens.sw, ylab = "Frequency", xlab = "Width", main = "Sepal Width Density")

dens.sl = density(iris$Petal.Length)
plot(dens.sl, ylab = "Frequency", xlab = "Length", main = "Sepal Length Density")

# Density plot by type
# ????

#dens.pw = density(iris$Petal.Width)
#plot(dens.pw, ylab = "Frequency", xlab = "Width", main = "Petal Width Density")

dens.pw = density(iris$Petal.Width)
 
dens_setosa.pw = density(iris$Petal.Width[iris$Species=="setosa"])
plot(dens_setosa.pw,ylab = "Frequency", xlab = "Width", main = "setosa Petal Width Density")
install.packages("ggplot2")
library(ggplot2)
ggplot(iris$Petal.Width) + geom_density(aes(x=iris$Species))
# ??? how to plot density by group in the same plot
 
# Countour Plot - a graph that explores the potential relatioship among three variables
# ????
library(MASS)
petal.dens=kde2d(iris$Petal.Length, iris$Petal.Width)
contour(petal.dens)
# heat map
petal.dens=kde2d(iris$Petal.Length, iris$Petal.Width)
image(petal.dens)

#qqplot - use the Shapiro-Wilk normality test; Shapiro-Wilk test: if p-value > 0.05, then data are normally distributed
par(mfrow = c(1,3))
ps = seq(0,1,length=25)
quantile.virginica=quantile(iris$Petal.Length[iris$Species=="virginica"], probs=ps)
qqnorm(quantile.virginica, main = "Virginica")
qqline(quantile.virginica)

quantile.versicolor=quantile(iris$Petal.Length[iris$Species=="versicolor"], probs=ps)
qqnorm(quantile.versicolor, main = "Versicolor")
qqline(quantile.versicolor)

quantile.setosa=quantile(iris$Petal.Length[iris$Species=="setosa"], probs=ps)
qqnorm(quantile.setosa, main = "Setosa")
qqline(quantile.setosa)
# Shapiro-Wilk test
shapiro.test(quantile.setosa)
shapiro.test(quantile.virginica)
shapiro.test(quantile.versicolor)

#sample size
#bootstrapping
times=(0:59)
counts = c(49,51,50,85,47,61,29,29,32,21,36,38,30,27,
           24,34,38,37,24,32,32,36,23,27,22,19,10,10,
           12,13,8,12,5,6,6,1,11,2,4,1,2,4,0,0,0,0,0,
           0,0,0,0,0,0,1,1,6,2,3,4,0)
table(counts)
length(counts)
visits = rep.int(times,counts)
mean.age=function(n) {
  trials = 1000
  my.samples=matrix(sample(visits, size=n*trials,replace=TRUE), trials)
  means=apply(my.samples, 1, mean)
  means
}
par(mfrow=c(1,2))
# 1 people out of the sample at the time
hist(mean.age(1), main="Mean of 1 Visit", xlab="Number of Visits")
hist(visits, main = "ER visits Data", xlab ="Number of Visits")
par(mfrow=c(1,2))
hist(mean.age(2), main="Mean of 1 Visit", xlab="Number of Visits")
hist(visits, main = "ER visits Data", xlab ="Number of Visits")

par(mfrow=c(1,2))
hist(mean.age(20), main="Mean of 1 Visit", xlab="Number of Visits")
hist(visits, main = "ER visits Data", xlab ="Number of Visits")
# created a normal distribution when large numbers of sample at the time.
par(mfrow=c(1,2))
hist(mean.age(200), main="Mean of 1 Visit", xlab="Number of Visits")
hist(visits, main = "ER visits Data", xlab ="Number of Visits")

#as we increase our sample size, the histogram is more bell shaped and narrowed
# WE see from the historgrams that the distribution of the sample mean, gets narrowers as sample size increases.teh variance decreased
# 
par(mfrow=c(2,3))
MA1=mean.age(1)
MA2=mean.age(2)
MA10=mean.age(10)
MA20=mean.age(20)
MA100=mean.age(100)
MA200=mean.age(200)
hist(MA1, xlim=c(0,60))
hist(MA2, xlim=c(0,60))
hist(MA10, xlim=c(0,60))
hist(MA20, xlim=c(0,60))
hist(MA100, xlim=c(0,60)) 
hist(MA200, xlim=c(0,60))
# the variance to 1/n
vars <- data.frame(n=c(1,2,10,20,100,200), variance= c(var(MA1),var(MA2), var(MA10), var(MA20), var(MA100), var(MA200)))
vars
plot(vars$n, vars$variance)
# qqplot for every of the samples to check if it's normal distrubtions.
# If the points of a Q-Q plot fall o a stratight line, this indicates the sample data are sonsistent with the proposed theoretical distribution

par(mfrow=c(2,3))
qqnorm(MA1)
qqnorm(MA2)
qqnorm(MA10)
qqnorm(MA20)
qqnorm(MA100)
qqnorm(MA200)

# use the shapiro-wilk normal test p-value is greate than 0.5 - then is normaly distributed
par(mfrow=c(2,3))
shapiro.test(MA1)
shapiro.test(MA2)
shapiro.test(MA10)
shapiro.test(MA20)
shapiro.test(MA100)
shapiro.test(MA200)

# ???what the W means in shapiro-wilk

# statistic is a value computed from data - our average of ER visits;
# Random variable - number associated with outcome of an experiment; it has a distribution, a mean, and a variance.. 
# statistic is a random variable - it has a distribution, a mean and a variance

# in the field of statistics, we gather a subset of data from a population; use statistics for that sample; 
# draw conclusions about the population

# mean; standard deviation are the most common way to describe a population; 
# mean: the center of the distribution; 
# standard deviation: the spread
# Central Limit Theorem: distribution of a sample mean will alwats approach a normal distribution
# As sample size increases, any distribution will have a mean tends to behave normally
# normal distribution occurs so ofen in lives, it behavious very predictablly. 68% data in 1 deviation; 95% in 2 deviation; 
# 99% in 3 deviation
# The central limit theorem is the foundation of statistical inferences
# Binomal distribution
# Suppose each person in a 3000 person stadium has a 60% chance to be rooting for the home team. 
# What's the probability that more than 1850 people are rooting for the home team?
# binomial distribution - to calculate the probability of the binomial is very tidious. 
# The binomial is discrete; the normal distribution is continuous; 
# We use the proximate the binomial distribution to nomal distribution - we called "continuity correction"
# 
x = pnorm(1.8820239, lower.tail = FALSE)
#
1-pbinom(1850, 3000, .60)
# Statistics inference is use your dataset ot extract information about populations and anser real-world questions with near certainty

install.packages("ggplot2")

