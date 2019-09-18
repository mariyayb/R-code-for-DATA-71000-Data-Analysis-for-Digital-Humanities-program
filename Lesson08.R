
#install.packages("ggplot2")
library(ggplot2)


#####################
#Normal distribution
#####################

#some plots of t distributions and standard normal distribution
ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), aes(col='normal')) +
  stat_function(fun = dt, args = list(df=2), aes(col='df=02')) +
  stat_function(fun = dt, args = list(df=3), aes(col='df=03')) +
  stat_function(fun = dt, args = list(df=10), aes(col='df=10')) +
  stat_function(fun = dt, args = list(df=30), aes(col='df=30')) +
   scale_colour_manual("Legend", values = c("normal"="red", "df=02"="purple", "df=03"="blue","df=10"="green","df=30"="orange"))

#probabilities
#probability that a standard normal r.v. is below 3
pnorm(3)
#probability that a t r.v. is below 3 with df=2
pt(3,df=2)
#probability that a t r.v. is below 3 with df=25
pt(3,df=25)
#probability that a standard normal r.v. is below 1
pnorm(1)
#probability that a t r.v. is below 1 with df=2
pt(1,df=2)
#prob that a t r.v. is above -1, df=2
pt(-1,df=2,lower.tail=FALSE)


#value below which standard normal r.v. is with 0.9 probability
qnorm(.9)
#value below which t r.v. is with 0.9 probability, df=2
qt(.9,df=2)

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='red')+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(-4,qnorm(.9)),
                geom = "area", fill = "pink", alpha = .5)+
  stat_function(fun = dt, args = list(df = 2), col='yellow')+
  stat_function(fun = dt, args = list(df=2), xlim = c(-4,qt(.9,df=2)),
                geom = "area", fill = "yellow", alpha = .2)


###################################
#Confidence intervals for the mean
#Hypothesis testing for the mean
###################################

#use t distribution for: small sample size if data comes from
#normally distributed population
#degrees of freedom = n-1, where n is the sample size

#necessary assumptions for t test:
#observations are independent (SRS and < 10% of population)
#data is from a normal distribution if the sample is small

#open from NYC Open Data
# https://data.cityofnewyork.us/Recreation/-Kids-In-Motion-Playground-Programming/8p6c-94pc

#Attendance information for the "Kids in Motion" program, 
#which provides free activities in NYC’s playgrounds, 
#including organized sports, games, fitness demos, and board games.

kids <- read.csv("Kids_In_Motion.csv")

#names of columns

names(kids)

attendance <- kids$Total.Attendance
length(attendance)

attendanceTrueMu <- mean(attendance)
#attendanceTrueMu

#Let all amounts in this data set be the population
#Take a simple random sample of 25 job postings

attendanceSample <- sample(attendance, 25)
mean(attendanceSample)

#quantile quantile plot
ggplot() + aes(sample=attendanceSample)+stat_qq()

#only those events with attendance < 500
attendanceSmall <- attendance[attendance < 500]
length(attendanceSmall)

#take a sample
attSmallSample <- sample(attendanceSmall, 25)
#quantile quantile plot
ggplot() + aes(sample=attSmallSample)+stat_qq()


#Plot a histogram of the sample data
ggplot() + aes(attSmallSample) +
  geom_histogram(binwidth=100,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(attSmallSample), sd = sd(attSmallSample)) *100* length(attSmallSample) ,
                color = "purple", size = 1)

#create a 95% confidence interval
#for the population mean mu
#using the t distribution

#since the sample size n = 25, use 
#df = 25 - 1 = 24

#point estimate for mu is the sample average, xBar

xBar <- mean(attSmallSample)

#standard error = sample SD / sqrt(n)

SE <- sd(attSmallSample)/sqrt(25)

#critical t value

tCritical <- qt(.95 + .05/2,df=24)

#the confidence interval
minCI <- xBar - tCritical*SE
maxCI <- xBar + tCritical*SE

minCI
maxCI

#the true mu
mean(attendanceSmall)


#Conduct a one-sample hypothesis t-test for mu
#Let H_0: mu = 250
#alpha = 0.05


############################################################
#Paired vs. unpaired data
############################################################

#We will test whether there is a difference
#between Tues and Thurs attendance using 
#the same sample in two ways:
#using a paired and unpaired t-test

#sample of 25 events
sampleIndices <- sample(dim(kids)[1],25)


############
#Paired data
############

#Consider attendance at each event on Thursdays vs. Tuesdays
#Is there a difference?
#First subtract the Thursday attendance from the Tuesday attendance,
#so that there is one column of data
#Test using H_0: muDifference = 0

#differente in attendance for all events
difference <- kids$Tuesday.s.Attendance - kids$Thursday.s.Attendance

#difference in attendance for the sample
diffSample <- difference[sampleIndices]

#t-score: t = (xBar - mu0)/(s/sqrt(n))

xBar <- mean(diffSample)
mu0 <- 0
s <- sd(diffSample)
n <- 25



t <- (xBar - mu0)/(s/sqrt(n))
t

#p-value (this is a two-sided test)
pPaired <- 2*pt(abs(t),df=n-1,lower.tail=FALSE)
pPaired

?t.test

##############
#Unpaired data
##############

#use the same sample for an unpaired t-test
#we will not take the difference in attendance
#and treat the two days as unpaired,
#thus, we are using two samples (Tues attendance and Thurs attendance)

#Consider average attendance on Thursdays vs. Tuesdays (unpaired)
#Is there a difference?
#Test using H_0: muTh = muTues
#This is equivalent to H_0: muTh - muTues = 0


xBarTh <- mean(kids$Thursday.s.Attendance[sampleIndices])
sTh <- sd(kids$Thursday.s.Attendance[sampleIndices])

xBarTues <- mean(kids$Tuesday.s.Attendance[sampleIndices])
sTues <- sd(kids$Tuesday.s.Attendance[sampleIndices])

#t-score: t = (xBar - mu0)/sqrt(sTh^2/sqrt(nTh) + sTues^2/sqrt(nTues))

xBar <- xBarTh - xBarTues
mu0 <- 0
nTh <- 25
nTues <- 25
denominator <- sqrt(sTh^2/nTh+sTues^2/nTues)
SE <- denominator #this is the standard error of the sampling distribution of the difference in means
#DF <- min(nTh-1,nTues-1)
#The better choice for degrees of freedom
DF <- (SE)^4/(sTh^4/((nTh-1)*nTh^2) + sTues^4/((nTues-1)*nTues^2))



t <- (xBar - mu0)/denominator
t

#p-value (this is a two-sided test)
pUnpaired <- 2*pt(abs(t),df=DF,lower.tail=FALSE)
pUnpaired


#load teacher data

teacher <- read.delim("teacher.txt")

names(teacher)

#base salaries of teachers with MA
baseMA <- teacher$base[teacher$degree == "MA"]
baseMA

#base salaries of teacher with BA
baseBA <- teacher$base[teacher$degree == "BA"]
baseBA

#Perform an unpaired t-test on the average salaries
#This data represents our sample
#What is the appropriate population?

xBarMA <- mean(baseMA)
sMA <- sd(baseMA)

xBarBA <- mean(baseBA)
sBA <- sd(baseBA)

#t-score: t = (xBar - mu0)/sqrt(sMA^2/sqrt(nMA) + sBA^2/sqrt(nBA))

xBar <- xBarMA - xBarBA
mu0 <- 0
nMA <- length(baseMA)
nBA <- length(baseBA)
denominator <- sqrt(sMA^2/nMA+sBA^2/nBA)
DF <- min(nMA-1,nBA-1)

t <- (xBar - mu0)/denominator
t

#p-value (this is a two-sided test)
p <- 2*pt(abs(t),df=DF,lower.tail=FALSE)
p

#What changes for a one-sided test?

#degrees of freedom: a better choice than min(n_1-1, n_2-1) is
# DF = (SE)^4/(s1^4/((n1-1)*n1^2) + s2^4/((n2-1)*n2^2))


#Hypothesis test
