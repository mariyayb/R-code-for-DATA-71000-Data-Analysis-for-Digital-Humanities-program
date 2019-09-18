
#install.packages("ggplot2")
library(ggplot2)
library(grid)

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
  geom_histogram(binwidth=0.05,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(height), sd = sd(height)) *0.05* length(height) ,
                color = "purple", size = 1)

#take a simple random sample from height and from height, each
#of size 100 and compute the average
samp_height <- sample(height,100)
sampAve_height <- mean(samp_height)
sampAve_height

samp_height <- sample(height,100)
sampAve_height <- mean(samp_height)
sampAve_height

#repeat sampling 50 times
sampDist_height <- sapply(1:50, function(x) sample(height,100))

#rows are samples
sampDist_height <- t(sampDist_height)

#compute each sample average
meanDist_height <- rowMeans(sampDist_height)
meanDist_height

#plot a histogram
ggplot() + aes(meanDist_height) +
  geom_histogram(binwidth=.005,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(meanDist_height), sd = sd(meanDist_height)) *0.005* length(meanDist_height) ,
                color = "purple", size = 1)


#repeat sampling 500 times
sampDist_height <- sapply(1:500, function(x) sample(height,100))

#rows are samples
sampDist_height <- t(sampDist_height)

#compute each sample average
meanDist_height <- rowMeans(sampDist_height)
meanDist_height

mean(meanDist_height)

#plot a histogram
ggplot() + aes(meanDist_height) +
  geom_histogram(binwidth=.005,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(meanDist_height), sd = sd(meanDist_height)) *0.005* length(meanDist_height) ,
                color = "purple", size = 1)

#quantile quantile plot
ggplot() + aes(sample=meanDist_height)+stat_qq()

#repeat sampling 100000 times
sampDist_height <- sapply(1:100000, function(x) sample(height,100))

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

#Calculate a 99% confidence interval
#Step 0: take a sample and find the sample  mean (point estimate)
sample <- sample(height,100)
xBar <- mean(sample)
xBar
#Step 1: find z*
zStar <- qnorm(.99 + (1-.99)/2)
zStar

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='red')+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(-zStar,zStar),
                geom = "area", fill = "pink", alpha = .5) +
  geom_text(x=0, y=0.2, label="0.99", colour="darkred",size=8)


#Step 2: find z* times SE (margin of error)
margin<-zStar*se
margin

#Step 3: the CI is the point estimate =/- the margin of error
min<- xBar - margin
max<- xBar + margin
CI <- c(min,max)
CI

#does CI contain the true population mean?
mean(height)

###########################
#Confidence intervals
###########################
#95% CI for the mean: sample average +/- 1.96*SE

minimum <- meanDist_height - 1.96*se
maximum <- meanDist_height + 1.96*se

CI <- rbind(1:50,minimum[1:50],meanDist_height[1:50],maximum[1:50])
CI <- t(CI)
CI <-data.frame(CI)
colnames(CI) <- c("number","min","ave","max")
CI



#plot CIs
ggplot(CI, aes(y=ave, x=number)) + 
  geom_pointrange(aes(ymin=min, ymax=max)) +
  geom_hline(yintercept=mean(height), linetype="dashed", color = "red")


CI <- rbind(51:100,minimum[51:100],meanDist_height[51:100],maximum[51:100])
CI <- t(CI)
CI <-data.frame(CI)
colnames(CI) <- c("number","min","ave","max")
CI



#plot CIs
ggplot(CI, aes(y=ave, x=number)) + 
  geom_pointrange(aes(ymin=min, ymax=max)) +
  geom_hline(yintercept=mean(height), linetype="dashed", color = "red")

CI <- rbind(101:150,minimum[101:150],meanDist_height[101:150],maximum[101:150])
CI <- t(CI)
CI <-data.frame(CI)
colnames(CI) <- c("number","min","ave","max")
CI



#plot CIs
ggplot(CI, aes(y=ave, x=number)) + 
  geom_pointrange(aes(ymin=min, ymax=max)) +
  geom_hline(yintercept=mean(height), linetype="dashed", color = "red")


CI <- rbind(151:200,minimum[151:200],meanDist_height[151:200],maximum[151:200])
CI <- t(CI)
CI <-data.frame(CI)
colnames(CI) <- c("number","min","ave","max")
CI



#plot CIs
ggplot(CI, aes(y=ave, x=number)) + 
  geom_pointrange(aes(ymin=min, ymax=max)) +
  geom_hline(yintercept=mean(height), linetype="dashed", color = "red")



############################
############################
############################

#load email.csv data set
email<-read.csv("email.csv")

#summary statisics
summary(email)

num_char <- email$num_char
line_breaks <- email$line_breaks

mean_num_char <- mean(num_char)
mean_num_char
mean_line_breaks <- mean(line_breaks)
mean_line_breaks

#plot a histogram of num_char
ggplot() + aes(num_char) +
  geom_histogram(binwidth=2,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(num_char), sd = sd(num_char)) *2* length(num_char) ,
                color = "purple", size = 1)

#take a simple random sample from num_char and from line_breaks, each
#of size 100 and compute the average
samp_num_char <- sample(num_char,100)
sampAve_num_char <- mean(samp_num_char)
sampAve_num_char

samp_line_breaks <- sample(line_breaks,100)
sampAve_line_breaks <- mean(samp_line_breaks)
sampAve_line_breaks

#repeat sampling 50 times
sampDist_num_char <- sapply(1:50, function(x) sample(num_char,100))

#rows are samples
sampDist_num_char <- t(sampDist_num_char)

#compute each sample average
meanDist_num_char <- rowMeans(sampDist_num_char)
meanDist_num_char

#plot a histogram
ggplot() + aes(meanDist_num_char) +
  geom_histogram(binwidth=.5,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(meanDist_num_char), sd = sd(meanDist_num_char)) *0.5* length(meanDist_num_char) ,
                color = "purple", size = 1)


#repeat sampling 500 times
sampDist_num_char <- sapply(1:500, function(x) sample(num_char,100))

#rows are samples
sampDist_num_char <- t(sampDist_num_char)

#compute each sample average
meanDist_num_char <- rowMeans(sampDist_num_char)
meanDist_num_char

mean(meanDist_num_char)

#plot a histogram
ggplot() + aes(meanDist_num_char) +
  geom_histogram(binwidth=.5,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(meanDist_num_char), sd = sd(meanDist_num_char)) *0.5* length(meanDist_num_char) ,
                color = "purple", size = 1)

#quantile quantile plot
ggplot() + aes(sample=meanDist_num_char)+stat_qq()

#repeat sampling 100000 times
sampDist_num_char <- sapply(1:100000, function(x) sample(num_char,100))

#rows are samples
sampDist_num_char <- t(sampDist_num_char)

#compute each sample average
meanDist_num_char <- rowMeans(sampDist_num_char)
meanDist_num_char

mean(meanDist_num_char)

#plot a histogram
ggplot() + aes(meanDist_num_char) +
  geom_histogram(binwidth=.5,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(meanDist_num_char), sd = sd(meanDist_num_char)) *0.5* length(meanDist_num_char) ,
                color = "purple", size = 1)

#quantile quantile plot
ggplot() + aes(sample=meanDist_num_char)+stat_qq()


#standard deviation of the sampling distribution vs standard error (SD of the sampling distribution)
sdSamp <- sd(meanDist_num_char)
sdSamp 

#standard error: population sd / sqrt (100)
se <- sd(num_char)/10
se

#Calculate a 99% confidence interval
#Step 0: take a sample and find the sample  mean (point estimate)
sample <- sample(num_char,100)
xBar <- mean(sample)
xBar
#Step 1: find z*
zStar <- qnorm(.99 + (1-.99)/2)
zStar

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='red')+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(-zStar,zStar),
                geom = "area", fill = "pink", alpha = .5) +
                geom_text(x=0, y=0.2, label="0.99", colour="darkred",size=8)
             
  
#Step 2: find z* times SE (margin of error)
margin<-zStar*se
margin

#Step 3: the CI is the point estimate =/- the margin of error
min<- xBar - margin
max<- xBar + margin
CI <- c(min,max)
CI

#does CI contain the true population mean?
mean(num_char)


###########################
#Confidence intervals
###########################
#95% CI for the mean: sample average +/- 1.96*SE

minimum <- meanDist_num_char - 1.96*se
maximum <- meanDist_num_char + 1.96*se

CI <- rbind(1:50,minimum[1:50],meanDist_num_char[1:50],maximum[1:50])
CI <- t(CI)
CI <-data.frame(CI)
colnames(CI) <- c("number","min","ave","max")
CI



#plot CIs
ggplot(CI, aes(y=ave, x=number)) + 
  geom_pointrange(aes(ymin=min, ymax=max)) +
  geom_hline(yintercept=mean(num_char), linetype="dashed", color = "red")


CI <- rbind(51:100,minimum[51:100],meanDist_num_char[51:100],maximum[51:100])
CI <- t(CI)
CI <-data.frame(CI)
colnames(CI) <- c("number","min","ave","max")
CI



#plot CIs
ggplot(CI, aes(y=ave, x=number)) + 
  geom_pointrange(aes(ymin=min, ymax=max)) +
  geom_hline(yintercept=mean(num_char), linetype="dashed", color = "red")

CI <- rbind(101:150,minimum[101:150],meanDist_num_char[101:150],maximum[101:150])
CI <- t(CI)
CI <-data.frame(CI)
colnames(CI) <- c("number","min","ave","max")
CI



#plot CIs
ggplot(CI, aes(y=ave, x=number)) + 
  geom_pointrange(aes(ymin=min, ymax=max)) +
  geom_hline(yintercept=mean(num_char), linetype="dashed", color = "red")


CI <- rbind(151:200,minimum[151:200],meanDist_num_char[151:200],maximum[151:200])
CI <- t(CI)
CI <-data.frame(CI)
colnames(CI) <- c("number","min","ave","max")
CI



#plot CIs
ggplot(CI, aes(y=ave, x=number)) + 
  geom_pointrange(aes(ymin=min, ymax=max)) +
  geom_hline(yintercept=mean(num_char), linetype="dashed", color = "red")


###########################
#Hypothesis testing
###########################
#load yrbss.csv data set
email<-read.csv("yrbss.csv")



###########################
#Homework notes
###########################
#General comments:

#Include your reasoning if you answer yes/no or just one or two words

#Show work for any calculations

#when identifying the population, include 
#the geographic region. Some studies might apply 
#to populations only in a 
#particular country or geographic area

#Be careful when identifying sample vs. population

#Some questions to think about:
#Is a simple random sample always representative of a population?
#How can we ensure a sample is unbiased?
#Is a nonrandom sample representative of a population?

#1.23 Since the researchers are not merely observing,
#this falls more in the category of an experiment
#A separate question is whether the experimental design allows for 
#conclusions about causal associations
#A treatment is applied, but it is applied to the whole group here,
#so this experiment is not randomized


#1.25a The problem with the conclusion is that parents
#who have more free time to spend with kids are probably
#those who responded. Most of the parents who are busier likely did 
#not respond at all.
#1.25b Those mothers who could not be reached (almost half) could largely
#fall into one category or the other.
#For example, if all those who could not be reached are smokers,
#and all those who could be reached are non-smokers, no 
#conclusions can be drawn
#1.25c The doctor did not include any patients with joint problems. 
#Maybe of those with joint problems, all (3 in every 3) are runners -- there is no way to tell
#This is a small biased sample that does not represent the population of patients even

