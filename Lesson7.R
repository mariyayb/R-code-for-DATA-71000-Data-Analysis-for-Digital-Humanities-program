
#install.packages("ggplot2")
library(ggplot2)


#####################
#Inference
#####################

#load yrbss.csv data set
yrbss<-read.csv("yrbss.csv")

names(yrbss) #names of data columns
dim(yrbss) #dimensions

height<-na.omit(yrbss$height)
length(height)

#plot a histogram of height
ggplot() + aes(height) +
  geom_histogram(binwidth=0.03,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(height), sd = sd(height)) *0.03* length(height) ,
                color = "purple", size = 1)

#take a simple random sample from height and from height, each
#of size 100 and compute the average
samp_height <- sample(height,100)
sampAve_height <- mean(samp_height)
sampAve_height

samp_height <- sample(height,100)
sampAve_height <- mean(samp_height)
sampAve_height

#repeat sampling 10000 times
sampDist_height <- sapply(1:10000, function(x) sample(height,100))

#rows are samples
sampDist_height <- t(sampDist_height)

#compute each sample average
meanDist_height <- rowMeans(sampDist_height)
meanDist_height

mean(meanDist_height)

#plot a histogram
ggplot() + aes(meanDist_height) +
  geom_histogram(binwidth=.001,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(meanDist_height), sd = sd(meanDist_height)) *0.001* length(meanDist_height) ,
                color = "purple", size = 1)

#quantile quantile plot
ggplot() + aes(sample=meanDist_height)+stat_qq()

#standard deviation of the sampling distribution vs standard error (SD of the sampling distribution)
sdSamp <- sd(meanDist_height)
sdSamp 

#standard error: population sd / sqrt (100)
se <- sd(height)/10
se

#try again for a larger sample
#repeat sampling 10,000 times for sample size 1000
sampDist_height <- sapply(1:10000, function(x) sample(height,1000))

#rows are samples
sampDist_height <- t(sampDist_height)

#compute each sample average
meanDist_height <- rowMeans(sampDist_height)
meanDist_height

mean(meanDist_height)

#plot a histogram
ggplot() + aes(meanDist_height) +
  geom_histogram(binwidth=.001,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(meanDist_height), sd = sd(meanDist_height)) *0.001* length(meanDist_height) ,
                color = "purple", size = 1)

#quantile quantile plot
ggplot() + aes(sample=meanDist_height)+stat_qq()

#standard deviation of the sampling distribution vs standard error (SD of the sampling distribution)
sdSamp <- sd(meanDist_height)
sdSamp 

#standard error: population sd / sqrt (1000)
se <- sd(height)/sqrt(1000)
se

############################
############################
############################

#Repeat above for weight; see the sampling distribution of the mean

weight<-na.omit(yrbss$weight)
length(weight)

#sort the weights in increasing, then decreasing order and 
#return the first five entries
sort(weight)[1:5]
sort(weight,decreasing=TRUE)[1:5]


#plot a histogram of weight
ggplot() + aes(weight) +
  geom_histogram(binwidth=5,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(weight), sd = sd(weight)) *5* length(weight) ,
                color = "purple", size = 1)

#take a simple random sample from weight and from weight, each
#of size 100 and compute the average
samp_weight <- sample(weight,100)
sampAve_weight <- mean(samp_weight)
sampAve_weight

samp_weight <- sample(weight,50)
sampAve_weight <- mean(samp_weight)
sampAve_weight

#repeat sampling 10000 times
sampDist_weight <- sapply(1:10000, function(x) sample(weight,50))

#rows are samples
sampDist_weight <- t(sampDist_weight)

#compute each sample average
meanDist_weight <- rowMeans(sampDist_weight)
meanDist_weight

mean(meanDist_weight)

#plot a histogram
ggplot() + aes(meanDist_weight) +
  geom_histogram(binwidth=.5,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(meanDist_weight), sd = sd(meanDist_weight)) *.5* length(meanDist_weight) ,
                color = "purple", size = 1)

#quantile quantile plot
ggplot() + aes(sample=meanDist_weight)+stat_qq()

#try again for a larger sample
#repeat sampling 10,000 times for sample size 1000
sampDist_weight <- sapply(1:10000, function(x) sample(weight,1000))

#rows are samples
sampDist_weight <- t(sampDist_weight)

#compute each sample average
meanDist_weight <- rowMeans(sampDist_weight)
meanDist_weight

mean(meanDist_weight)

#plot a histogram
ggplot() + aes(meanDist_weight) +
  geom_histogram(binwidth=.1,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(meanDist_weight), sd = sd(meanDist_weight)) *0.1* length(meanDist_weight) ,
                color = "purple", size = 1)

#quantile quantile plot
ggplot() + aes(sample=meanDist_weight)+stat_qq()

#standard deviation of the sampling distribution vs standard error (SD of the sampling distribution)
sdSamp <- sd(meanDist_weight)
sdSamp 

#standard error: population sd / sqrt (1000)
se <- sd(weight)/sqrt(1000)
se

###
#Note that visually we see the distribution 
#of heights is more symmetric, whereas the 
#distribution of weights has more skew
#For which sample size is the distribution 
#of average weights closer to normal?
###


######################################################
### Hypothesis Testing for the population mean ###
######################################################

#open NYC_jobs from NYC Open Data
# https://data.cityofnewyork.us/City-Government/NYC-Jobs/kpav-sd4t

jobs <- read.csv("NYC_Jobs.csv")

#names of columns

names(jobs)

salaryTo <- jobs$Salary.Range.To
#salaryTo <- salaryTo[salaryTo > 30000]
salaryFrom <- jobs$Salary.Range.From
#salaryFrom <- salaryFrom[salaryFrom != 0]

salaryToMu <- mean(salaryTo)
salaryToMu
salaryFromMu <-mean(salaryFrom)

#Let all job postings in this data set be the population
#Take a simple random sample of 100 job postings

salaryToSample <- sample(salaryTo, 100)
mean(salaryToSample)

#Create a 95% confidence interval for the population mean mu
# sample mean +/- z * SE
xBar <- mean(salaryToSample) #sample mean
z <- qnorm(.95 + (1-.95)/2) #z score for 95% CI
SE <- sd(salaryTo)/sqrt(100) # standard error

min <- xBar - z*SE
max <- xBar + z*SE

#Does the confidence interval contain salaryToMu?
min
max

#Plot a histogram of the sample data
ggplot() + aes(salaryToSample) +
  geom_histogram(binwidth=5000,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(salaryToSample), sd = sd(salaryToSample)) *5000* length(salaryToSample) ,
                color = "purple", size = 1)


#Hypothesis test for the population mean mu (two-sided)
#What is the population?


#The null hypothesis: H_0 : mu = 75000
#The alternative hypothesis: H_A : mu != 75000 (is not equal to)

#The significance level alpha = (small) probability, often 0.05,
#of rejecting the null hypothesis when it is true
#alpha is determined at the beginning, before the sample 
#is analyzed

#If the probability of obtaining the sample statistic
#that was obtained or one that is more favorable to 
#H_0 is less than alpha, reject H_0

#If the evidence overwhelmingly does not support H_0, reject 
#H_0 in favor of H_A; otherwise, fail to reject H_0
#but we do not accept H_0

#If the probability of obtaining the sample statistic
#that was obtained or one that is more favorable to 
#H_A is less than alpha, reject H_0
#This probability p is called the p-value

#Throughout the test, assume H_0 is true
#Only at the end, reject H_0 if 
#there is enough evidence to do so,
#precisely when p < alpha

#Assuming mu = 75,000, since the sampling distribution
#for the mean is approximately normal with 
#standard error = sigma/sqrt(n), n being the sample size,
#with alpha = 0.05, reject H_0 if the sample average 
# is outside of a 95% confidence interval 

#Create a 95% confidence interval for the population mean mu
# sample mean +/- z * SE
xBar <- mean(salaryToSample) #sample mean
z <- qnorm(.95 + (1-.95)/2) #z score for 95% CI
SE <- sd(salaryTo)/sqrt(100) # standard error

min <- xBar - z*SE
max <- xBar + z*SE

#Does the confidence interval contain salaryToMu?
min
max

#If yes, do not reject H_0; If no, reject H_0 in favor of H_A

###########
# p-value #
###########

#for a two-sided test, the p-value is 
# 2*(probability of tail associated with z-score of sample average)
mu <- 75000 #according to H_0

#this is the general formula for the "test statistic"
#of a hypothesis test for the mean (as long as sigma is known)
z <- (xBar - mu)/SE 

z
pValue <- 2*pnorm(abs(z),lower.tail=FALSE)
pValue

#If pValue < alpha, reject H_0

#install.packages("BSDA")
library(BSDA)
??z.test

#the critical z value defines the "rejection region" and 
#is based on alpha
alpha <- 0.05
critical <- qnorm(1-alpha + alpha/2)
critical

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='red')+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(-4,-critical),
                geom = "area", fill = "pink", alpha = .5) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(critical,4),
                geom = "area", fill = "pink", alpha = .5) +
  geom_text(x=0, y=0.2, label=paste("1-","\u03B1"), colour="darkred",size=8)+
  geom_text(x=-3, y=0.1, label=paste("\u03B1","/2"), colour="darkred",size=6)+
  geom_text(x=3, y=0.1, label=paste("\u03B1","/2"), colour="darkred",size=6)+
  scale_x_continuous(name="z-scores",breaks=c(-critical,critical)) +
  annotate("segment", x = -3, xend = -2.8, y = 0.07, yend = 0.02, colour = "darkred", arrow=arrow())+
  annotate("segment", x = 3, xend = 2.8, y = 0.07, yend = 0.02, colour = "darkred", arrow=arrow())

#Rejection region for a one-sided hypothesis test


#The null hypothesis: H_0 : mu = 75000
#The alternative hypothesis: H_A : mu > 75000 

alpha <- 0.05
critical <- qnorm(1-alpha)
critical

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='red')+
#  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(-4,-critical),
#                geom = "area", fill = "pink", alpha = .5) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(critical,4),
                geom = "area", fill = "pink", alpha = .5) +
  geom_text(x=0, y=0.2, label=paste("1-","\u03B1"), colour="darkred",size=8)+
 # geom_text(x=-3, y=0.1, label=paste("\u03B1","/2"), colour="darkred",size=6)+
  geom_text(x=3, y=0.1, label=paste("\u03B1"), colour="darkred",size=6)+
  scale_x_continuous(name="z-scores",breaks=c(critical)) +
 # annotate("segment", x = -3, xend = -2.8, y = 0.07, yend = 0.02, colour = "darkred", arrow=arrow())+
  annotate("segment", x = 3, xend = 2.8, y = 0.07, yend = 0.02, colour = "darkred", arrow=arrow())


##############
### Errors ###
##############

#type one error: probability of rejecting H_0 when it is true
#this is equal to alpha

#type two error: probability of failing to reject H_0 
#when H_A is true
#sometimes referred to beta
#1-beta is called the "power" of the test

#the power of a test is the probability of correctly 
#rejecting H_0 in favor of H_A

#What affects the power?

mu0 <- 0
muA <- 2.5

ggplot(data.frame(x = c(-4, 6)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='red')+
  stat_function(fun = dnorm, args = list(mean = muA, sd = 1), col='purple')+
  stat_function(fun = dnorm, args = list(mean = muA, sd = 1), xlim = c(-4,critical),
                geom = "area", fill = "yellow", alpha = .5) +
  geom_text(x=1.3, y=0.1, label=paste("\u03B2"), colour="purple",size=6)+
  scale_x_continuous(name="",breaks=c(0,critical,muA),
                labels=c(paste("\u03bc"),expression(z["crit"]),
                expression(paste("\u03bc")["A"])))



ggplot(data.frame(x = c(-4, 6)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='red')+
  stat_function(fun = dnorm, args = list(mean = muA, sd = 1), col='purple')+
  stat_function(fun = dnorm, args = list(mean = muA, sd = 1), xlim = c(-4,critical),
                geom = "area", fill = "yellow", alpha = .5) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(critical,4),
                geom = "area", fill = "red", alpha = .5) +
  geom_text(x=1.3, y=0.1, label=paste("\u03B2"), colour="orange",size=6)+
  geom_text(x=2, y=0.03, label=paste("\u03B1"), colour="darkred",size=6)+
  scale_x_continuous(name="",breaks=c(0,critical,muA),
                     labels=c(paste("\u03bc"),expression(z["crit"]),
                              expression(paste("\u03bc")["A"])))

#plot values of alpha and beta

nSmall <- 20
nLarge <- 100


a <- c(0.01, seq(from = 0.05, to = 0.5, by=0.05)) #alphas
b <- pnorm(qnorm(1-a), mean= muA, sd=1)  #betas for each alpha
power <- 1-b                            #power for each alpha



#create data frame from above data
errors <- data.frame("alpha" = a, "beta" = b, "power" = power)
errors
#plot alpha vs. beta
ggplot(errors, aes(x=alpha, y=beta)) + geom_point()

#plot alpha vs. power
ggplot(errors, aes(x=alpha, y=power)) + geom_point()

