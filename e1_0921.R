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
# Statistics inference is use your dataset ot extract information about populations and anwser real-world questions with near certainty
# Build on the methods of descriptive statistics
# Draw conclusions about the population, based on data from the sample

install.packages("ggplot2")

data("Orange")
head(Orange)
table(Orange)
#point of estimate -which one is better, mean or median? The point of estimate gives us the perticulater point value; 
# The confidence interval estimate gives us the range of estimate
# statistical inference is the process of making an estimate, prediction, or dicsion about a population, based on a sample
# point estimator: a sttistic
# point estimate: a value
mean(Orange$age)
mean(Orange$circumference)

median(Orange$age)
median(Orange$circumference)

#confidence level and significance level
# 1. Draw conslusions about a population or process from sample data
# 2. Provide a statement of how much confidence we can place in our conclusions. 

# The most common point estimate is the sample mean X bar, which is used to estimate the poplulation mean u (mu)
data("women")
attach(women)
summary(women)
plot(height, weight)
#estimate median; mean and standard deviation
mean(height)
median(height)
mode(height)
sd(height)
mean(weight)
median(weight)
mode(weight)
sd(weight)

set.seed(1234)
x =cbind(rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1),
         rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1),
         rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1),
         rnorm(100,0,1))
apply(x,2,'mean')
apply(x,2, 'median')
# which point of estimate is more efficient, mean or median? Mean


round(mean(apply(x,2,'mean')),3)
round(mean(apply(x,2,'median')),3)

round(var(apply(x,2,'mean')),3)
round(var(apply(x,2,'median')),3)
# a point of estimate only gives a single number for a population parameter
# Just one point -- 
# the range of estimate??? confidence interval.. 
# Confidence interval draws inferences about a population by estimating an unknown paramerter; using an interval
# used to estimate population parameters using a range of values...
 # confidence level - how likely tht the population parameter is contained within the confidence interval. 


qnorm(0.05)
qnorm(0.025)
qnorm(0.005)
set.seed(343)
milk= 129-rexp(100000,0.95)
hist(milk, main="Histogram of Milk Population", col = "red")
true_mean=mean(milk)
true_sd=sd(milk)
true_mean
true_sd
#How close our data will be when we sample them from our 100000 jags of milk data?
# We can get a fairly accurate estimate of a large population by sampling a relatively smal subset of individuals

set.seed(343)
n=50
sample_milk = sample(milk, size=50, rep=T)
sample_mean = mean(sample_milk)
sample_mean
sample_mean - true_mean
sample_mean -1.96*sd(sample_milk)/sqrt(n)
sample_mean +1.96*sd(sample_milk)/sqrt(n)
boxplot(milk, sample_milk, main = "Population vs. Sample")
hist(sample_milk)

# Central Limit Theorem: no matter what the shape of the population distribution, 
# the shape of the distribution of the sample means is (approximately) normal

milk_mean=numeric(0)
for (i in 1:1000)
  milk_mean[i] =mean(sample(milk, 50, rep=T))
hist(milk_mean)

qqnorm(milk_mean)
qqline(milk_mean)
# calculate CI
samp_mean = numeric(0)
for (i in 1:100)
  samp_mean[i]= mean(sample(milk, 50, rep=T))
hist(samp_mean)
lines(density(samp_mean), lwd = 3, col="red")

#CI 90%
m = 20
n = 50
sigma = sd(milk)
SE = sigma/sqrt(n)
alpha = 0.10; zcrit=qnorm(1-alpha/2)
matplot(rbind(samp_mean[1:m]-zcrit*SE,samp_mean[1:m] + zcrit*SE),
        rbind(1:m, 1:m), type = "l", lty =1, lwd =2,
        col = "darkgray",
        xlab = "Ounces",
        ylab = "Confidence Intervals",
        main = "90% Conf. Int."); 
abline(v=mean(milk))

#CI 80% 
m = 20
n = 50
sigma = sd(milk)
SE = sigma/sqrt(n)
alpha = 0.20; zcrit=qnorm(1-alpha/2)
matplot(rbind(samp_mean[1:m]-zcrit*SE,samp_mean[1:m] + zcrit*SE),
        rbind(1:m, 1:m), type = "l", lty =1, lwd =2,
        col = "darkgray",
        xlab = "Ounces",
        ylab = "Confidence Intervals",
        main = "80% Conf. Int."); 
abline(v=mean(milk))
# We use qnorm(.975) to get the desired z critical value instead of qnorm(0.95) because the sibtribution has two tails.
# 18 out of 20 capture the ture mean; so remember to use large sample, not just one..
# we can say 95% of the time, we'll cover the true mean. Here we use data  the estimate the population parameter and with CI.
# Instead we use the hypothesis: data could reject the hypothesis.. 
# an educated guess about something iin the world.. something testable by experiment or observation. 
# quantitative way to evaluate the sample
# statistical hypothesis is a claim about population parameter; we start with two Hypothesis
# Null Hypothesis: a claim initially assumed to be true, a prior belief, the status quo, the current standard
# Alternative Hypothesis: The contradictory claim, the competing claim, new theory, an alternative standard.
# Decision Principle: We reject H0 in favor of HA if sample data show strong evidence that H0 is false. 
# Otherwise we do not reject H0. Statisticians also say: "we fail to reject H0" (??? means H0 is true???)

# Test Statistic: a function of sample data we use to make a decision(reject H0 or do not reject H0)
# Rejection region: the set of all test statistic values for which H0 will be rejected
# Decision rule: The null hypothesis(H0) will be rejeted if our test statistic falls in teh rejection region
# # innocent until proven guilty - the mean of the sample is equal to the mean of the population; 
# we need to show the mean of the sample is NOT equal to the mean of the population - to reject this hypothesis.. 
# if the mean is equal to the mean of the population, we fail to reject H0..

# Paired Test - test two samples are form the same distribution
data("chickwts")
summary(chickwts)
head(chickwts)
meat=chickwts[chickwts$feed=="meatmeal",1]
horse=chickwts[chickwts$feed=="horsebean",1]
meat
horse
boxplot(meat, horse, main = "Meatmeal      Horsebean")
qqnorm(meat)
qqline(meat)
qqnorm(horse)
qqline(horse)
# qqplot produces a QQ plot of two datasets. If these two datasets are all on one side of the redline here, 
# there is a factor that affect these two dataset. In the case the feed. If the feed doesn't affect the weight,
# the qqplot will show data randomly show on each side of the redline
qqplot(meat, horse, xlim=c(100, 420), xlab="Meatmeal", ylab="Horsebean", 
       ylim=c(100,420), pch=20, cex=2)
abline(a=0, b=1, col="red")

#Parallel Plot
plot(x=c(meat, horse), y=c(rep(0, length(meat)),rep(1, length(horse))), pch="*",
     ylim=c(-1,2),cex=3,ylab="",xlab="Weight",main="Parallel Plot")
text(x=300, y=-0.4, labels("Meat meal"), cex=1)
text(x=160,y =1.9, labels("Horsebean", cex=1)
abline(h=1); abline(h=0)

# from the visual plot, looked like the weights are related to the feed. Next we use statistics approach for testing it.
# Hypothesis test H0 mean of the meat is the same as mean of the horsebean

# Two sample t-test is used to determine if the means of two independent data samples are different from one another 
# It refers as Welch's t-test - 2 samples compared
# Student's test: 1 sample compared with a population
#.t.test - assume the data is normally distributed and are NOT related
mean.meat=mean(meat)
mean.horse=mean(horse)
sd.meat=sd(meat)/sqrt(length(meat))
sd.horse=sd(horse)/sqrt(length(horse))
T.stat =( mean.meat - mean.horse)/sqrt(sd.meat^2+sd.horse^2)
T.stat
# the result of t.test: t = 5.0594 p-value = 0.0001054 very low
t.test(meat, horse)

# Paired t-test helps compare two samples where observations from one dample can be paired with the other
# Before-and -after measurements on the same subjects
# compare two different treatments where the treatments are given to th same subjects
install.packages("PairedData")
library(PairedData)
data(IceSkating)
attach(IceSkating)
IceSkating
summary(IceSkating)
# Is there a difference in speed when the leg is extended vs flexed?
qqplot(Extension, Flexion, xlim=c(1.5,2.5), ylim=c(1.5,2.5), pch=20, cex=2)
abline(a=0, b=1, lwd=3, col="red")
with(IceSkating,plot(paired(Extension,Flexion),type="McNeil"))
# Paired t-test assumption: difference between the pairs has to follow a normal distribution
# in this case, the original data has no normal distribution constraint
#par(mfrow=c(1,3)) 
hist(Extension)
hist(Flexion)
hist(Extension-Flexion)
# better way 
with(IceSkating, qqnorm(Extension-Flexion))
with(IceSkating, qqline(Extension-Flexion))
# confirm that the difference between Externsion and Flexion is normally distributed
# what if differences between the pairs are NOT normal, DO NOT use this paired test. 
# use Wilcoxon singed-rank test. 
shapiro.test(Extension-Flexion)
d = Extension-Flexion
d
mean_diff =mean(Extension) - mean(Flexion)
mean_diff
mean(d)
# What's our null hypothesis? The null hypothesis we assume that mean(d) is equal to 0; 

# the alternative is mean(d) is not equal to 0
# We wanted to have the evidence strong enough to reject the difference is 0; this will be a two tailed test; 
# In R, the function pt give the vlaue of the t distribution at our calculated test statistic (T) with df = n-1
# Since this is a two sided test, multiply by 2 to get the p-value
pp_v = 2*(1-pt(2.9346765,6))
pp_v
t.test(Extension, Flexion, paired = TRUE)
# Do NOT do regular t-test on paired data as following - p-value showed high -0.3046 means we can't reject the null hypothesis
# at any significant level - the df is Not 6, it's 11.875
t.test(Extension, Flexion)
# in the case the difference between the pair is not normally distributed, use Wilcoxon signed-rank test
wilcox.test(Extension, Flexion, paired=TRUE)
# linear regression models and assumptions
rainfall = c(3.07,3.55,3.90,4.38,4.79,5.30,5.42,5.99,6.46,6.77)
wheat =c(78,82,85,91,92,96,97,104,111,119)
summary(cbind(rainfall,wheat))
par(mfrow=c(1,1))
plot(rainfall, wheat, main = "Rainfall vs Wheat")
# linear regression
# Describes how a response variable Y changes as an explanatory variable X changes
# Minimizes teh squared difference between each observatin and the fitted line

# squared difference vs absolute difference - the squard difference fits better for the slop and y intercept
wheat.mod = lm(wheat~rainfall)
summary(wheat.mod)
# from the output, wheat is regressed on rainfall. 
# check the residuals to see if there are outliers, the median is close to 0. We wanted to see the residuals normally distributed
# Coefficients is where i find our the y-intercept -Beta 0 hat 45.0835, Beta 1 hat 10.1585
# what's the wheat height with 5 inches rain
wheat5 = 45.0835 + 10.1585*5 
# what about 100 inches? :-)
# 
# plot the residuals
qqnorm(residuals(lm(wheat~rainfall)))
qqline(residuals(lm(wheat~rainfall)), lwd =2, col="Blue")
# then we need to access how well our model fits
data(mtcars)
summary(mtcars)
#plot(mtcars$wt, mtcars$mpg)
# visually see if the data have the linear relationship
plot(mpg ~ wt, data=mtcars)
mpg_model=lm(mpg ~ wt, data=mtcars)
summary(mpg_model)
#Check the residuals - if the median isbiclose to 0, min and max... is it normally distributed
# Coefficients intercept and slope.. mpg = 37.2851  -5.3445*wt
# this is the T test, 
qt(.0975,df=28)
pt(-9.559,df=28)
hist(mpg_model$residuals)
qqnorm(mpg_model$residuals)
qqline(mpg_model$residuals )
shapiro.test(mpg_model$residuals)
# need to add more predictors to our modeal
# collinearity - occurs when tow or more predictor variables are closely related to one another, or highly correlated

# multiple Linear with weight and horsepower..
mpg_model1= lm(mpg~wt+hp, data=mtcars)
summary(mpg_model1)
# how do our model changes when adding one more variable.. 
cor(mtcars$wt, mtcars$hp)
# cor((mtcars$wt, mtcars$hp)) 0.65 - highly correlated - little high

# collinear: eith highly correclated or contribute the smae information to the model
# add all predictor into the model
mpg_model1= lm(mpg~., data=mtcars)
summary(mpg_model1)
# look at the pvalue - it showed none of them are significant; the adjusted Rsqure went down; we might get the poor result.
# as number of explanatory variables increases, adjusted R square gets smaller the Rsqure.

pairs(mtcars[,c(1:4)])

pairs(mtcars[,c(5:7)])
pairs(mtcars[,c(1,3:7)], col='blue')
round(cor(mtcars[,c(1,3:7)]),2)
# overfit - use the principle of parsimorny - the simple the better. 
# start pruning; choose the variable has the highest adjusted Rsqure; P value is a rough meansure. 
mpg_model2 <- step(lm(mpg ~ ., data=mtcars))

#multiple Liner Regression
install.packages("MASS")
library(MASS)
data(Pima.tr)
head(Pima.tr)
pima=Pima.tr; summary(pima)
hist(pima$bp)
# not skewed
plot(density(pima$bp), main = "blood pressure")
# relationship between data
pairs(pima[1:4])
# you may see the outliers from the pairs, like skin
pairs(pima[5:8])
round(cor(pima[1:7]), 2)
# looks like the small correlation...0.66(bmi/skik), and 0.60(npreg/age)
lm1 <-lm(bmi~npreg+glu+bp+skin+ped+age+type, data=pima)
summary(lm1)
#Residuals are not normally distribution
#Min       1Q   Median       3Q      Max 
#-19.9065  -2.5723  -0.1412   2.6039  11.2664 

# looking for low P value - close to zero anything above .05 or .01 ; tells you that variable is not siginificant in the model
# in this case npreg, glu, bp, age.. we should NOT use them.. 
# We use stepwise regression - it lets us drop insignificant variables one by one.. 
lm2 <- lm(bmi~npreg+glu+bp+skin+ped+age+type, data=pima)
drop1(lm2, test="F")
# the Sum of sq -drop the  has the smallest sum of sq  
#  drop the one has the smallest RSS,
# drop the one has the smallest AIC
# F value is used to test significant; 
# drop the largest P value
# in this case we remove the npreg
lm3 <- lm(bmi~glu+bp+skin+ped+age+type, data=pima)
drop1(lm3, test="F")
# choose the next drop glu
lm4 <- lm(bmi~bp+skin+ped+age+type, data=pima)
drop1(lm4, test="F")
# skin is significant; then type; drop bp
lm5 <- lm(bmi~skin+ped+age+type, data=pima)
drop1(lm5, test="F")
# drop the age
lm6 <- lm(bmi~skin+ped+type, data=pima)
drop1(lm6, test="F")
# Now this is the final.. for model bmi, but we could modle glu
lm7 <- lm(glu~npreg+bmi+bp+skin+ped+age+type, data=pima)
summary(lm7)
# check the normality of the residuals
plot(lm7$fitted.values, lm7$residuals, pch=20)

#ANOVA - analysis of variance 
# compare means arcoss groups
# Anova requires variability of the groups to be homogenous(?)
# Anova requires numbers in the groups are proximately the same

require(stats)
require(graphics)
boxplot(weight~feed, data=chickwts, col="lightgray", main = "Chickwts data", ylab = "Weight in grams", 
        xlab="Type of Feed")
summary(chickwts)
# use the variable-width box plot to verify the size of each group
# create variable-width with boxplot
# example code here
names = c(rep("1", 15), rep("2", 80),rep("3", 7),rep("4", 36))
value = c(sample(2:5,15),replace=T), sample(4:10,80),replace=T),
          sample(1:7,7),replace=T), sample(3:8,36),replace=T))
data=data.frame(names,value)
propotion=table(data$names)/nrow(data)
boxplot(data$value ~ data$names, width=propotion,
        main = "Width Relative to Group Size, n = 15,80,7,36")