
#install.packages("ggplot2")
library(ggplot2)

#open from NYC Open Data
# https://data.cityofnewyork.us/Recreation/-Kids-In-Motion-Playground-Programming/8p6c-94pc

#Attendance information for the "Kids in Motion" program, 
#which provides free activities in NYC’s playgrounds, 
#including organized sports, games, fitness demos, and board games.

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


#histogram to check normality for small sample
ggplot() + aes(diffSample) +
  geom_histogram(binwidth=25,color="black") +
  stat_function(fun = function(x) dnorm(x, mean = mean(diffSample), sd = sd(diffSample)) *25* length(diffSample) ,
                color = "purple", size = 1)

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


#Sometimes, there is no choice between paired and unpaired.
#What is an example?

#When there is a choice, which test is better?


#################################################
###Categorical Data###
#################################################

#################################################
###Inference for a proportion###
#################################################

#Let p be a population proportion and 
#pHat a sample proportion from the population

#Conditions for normality for the distribution of pHat:
#1. independent observations
#(SRS and sample size < 10% of population is sufficient)
#2. np >= 10 and n(1-p)>=10,
#where n is the sample size (success-failure condition)

#Then E(pHat) = p
# The standard error: SD(pHat) = sqrt(p(1-p)/n)


#Confidence interval for p

#Replace p with pHat to test the success-failure condition

#Find a confidence interval for the proportion
#of events in Manhattan in the kids data

#Categories for the Borough variable
levels(kids$Borough)

n<-length(kids$Borough[kids$Borough == "Manhattan"])
d<-length(kids$Borough)

p <- n/d

#See function in R:
#for large samples
?prop.test
#for small samples
?binom.test

borough <- kids$Borough
#take a sample from borough of size 50
#will test whether the proportion of events 
#in Manhattan is 1/5
#H0 : p = 1/5
#HA : p != 1/5
#alpha = 0.05
#Check: np >= 10, n(1-p) >= 10

n<-50

boroughSample <- sample(borough,n)
pHat <- length(boroughSample[boroughSample == "Manhattan"])/n

SE <- sqrt(pHat*(1-pHat)/n)

#z-score
z <- (pHat - 0.2)/SE
z
#p-value
#two-sided test

pValue <- 2*pnorm(abs(z),lower.tail=FALSE)
pValue

#Test again for Staten Island
#H0: p = 0.2
#HA: p < 0.2
#alpha = 0.05


n<-50

boroughSample <- sample(borough,n)
pHat <- length(boroughSample[boroughSample == "Staten Island"])/n

SE <- sqrt(pHat*(1-pHat)/n)

#z-score
z <- (pHat - 0.2)/SE
z
#p-value
#one-sided test

pValue <- pnorm(z)
pValue

#What conclusion can we make?

#True proportion for Staten Island
p <- length(borough[borough == "Staten Island"])/length(borough)
p


#Would we expect p to really be 1/5 for Staten Island?










#Consider the population of 
#Staten Island vs. New York City (all 5 boroughs)

popSI <- 479458/8622698
popSI

#Test whether the proportion of events in
#Staten Island is consistant with the 
#population proportion

#H0: p = popSI
#HA: p != popSI
#alpha = 0.05


n<-50

boroughSample <- sample(borough,n)
pHat <- length(boroughSample[boroughSample == "Staten Island"])/n

SE <- sqrt(pHat*(1-pHat)/n)

#z-score
z <- (pHat - popSI)/SE
z
#p-value
#two-sided test

pValue <- 2*pnorm(abs(z),lower.tail=FALSE)
pValue

#What is the conclusion?


#Create a confidence interval for
#the true proportion of events 
#in Staten Island

n<-50

boroughSample <- sample(borough,n)
pHat <- length(boroughSample[boroughSample == "Staten Island"])/n

SE <- sqrt(pHat*(1-pHat)/n)

#z*
z <- qnorm(0.95 + 0.05/2) #95% confidence interval

#margin of error
z*SE

#confidence interval
min <- pHat - z*SE
max <- pHat + z*SE

min
max

