
#install.packages("ggplot2")
library(ggplot2)



#####################
#Probability
#####################

#Flip a coin, head = 1, tails = 0
outcomes <- c(1,0)

#proportion of heads in 20 flips
sum(sample(outcomes,20,replace=TRUE))/20

#proportion of heads in n flips
sampleN <- function(vector,n){
  sum(sample(vector,n,replace=TRUE))/n
}

#proportion of heads in 1 through 1000 flips
probability<-numeric(1000)
for (i in 1:1000){
  probability[i]<-sampleN(outcomes,i)
}

head(probability)
head(probability,20)

ggplot() + aes(x=1:1000, y=probability) + geom_point()

#####################
#Normal distribution
#####################

#some plots
ggplot(data.frame(x = c(-7, 7)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='red') +
  stat_function(fun = dnorm, args = list(mean = 3, sd = 1), col='purple') +
  stat_function(fun = dnorm, args = list(mean = -3, sd = 1), col='blue') +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 2), col='yellow') +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 3), col='green')

#probabilities
#probability that a standard normal r.v. is below 3
pnorm(3)
#prob that a standard normal r.v. is below -2
pnorm(-2)
#prob that a standard normal r.v. is above -2
pnorm(-2,lower.tail=FALSE)

pnorm(-2) + pnorm(-2,lower.tail=FALSE)
pnorm(2)

#help with pnorm
?pnorm
help(pnorm)

#prob that a normal r.v. with mu = 4, sd = 2 is below 5
pnorm(5,mean=4,sd=2)

#value below which standard normal r.v. is with 0.9 probability
qnorm(.9)

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='red')+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(-4,qnorm(.9)),
                geom = "area", fill = "pink", alpha = .2)



#value below which normal r.v. (mu=4,sd=2) is with 0.9 probability
qnorm(.9,mean=4,sd=2)
ggplot(data.frame(x = c(-3, 11)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 4, sd = 2), col='red')+
  stat_function(fun = dnorm, args = list(mean = 4, sd = 2), xlim = c(-3,qnorm(.9,mean=4,sd=2)),
                geom = "area", fill = "pink", alpha = .2)


#value above which normal r.v. (mu=4,sd=2) is with 0.9 probability
a<-qnorm(.9,mean=4,sd=2,lower.tail=FALSE)

ggplot(data.frame(x = c(-3, 11)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 4, sd = 2), col='red')+
  stat_function(fun = dnorm, args = list(mean = 4, sd = 2), xlim = c(a, 11),
                geom = "area", fill = "pink", alpha = .2)


##########################
#Testing Normality
##########################

#load the oscars data file
oscars <- read.delim("oscars.txt")
summary(oscars)

#histogram of age in the oscar data set (experiment with binwidth)
ggplot(data=oscars, aes(x=age)) +
  geom_histogram(binwidth=5,color="black") +
  geom_vline(aes(xintercept=mean(age)), #dashed yellow line at mean
             color="yellow", linetype="dashed")

age <- oscars$age
m <-mean(age)
s <-sd(age)
#add a normal curve
ggplot(data=oscars, aes(x=age)) +
  geom_histogram(binwidth=5,color="black") +
  geom_vline(aes(xintercept=mean(age)), #dashed yellow line at mean
             color="yellow", linetype="dashed") +
  stat_function(fun = function(x) dnorm(x, mean = mean(oscars$age), sd = sd(oscars$age)) * length(oscars$age)*5 ,
                color = "purple", size = 1)


#quantile quantile plot
ggplot() + aes(sample=oscars$age)+stat_qq()

#histogram of birth_d in the oscar data set (experiment with binwidth)
ggplot(data=oscars, aes(x=birth_d)) +
  geom_histogram(binwidth=1,color="black") +
  geom_vline(aes(xintercept=mean(birth_d)), #dashed yellow line at mean
             color="yellow", linetype="dashed") 

#add a normal curve
ggplot(data=oscars, aes(x=birth_d)) +
  geom_histogram(binwidth=1,color="black") +
  geom_vline(aes(xintercept=mean(birth_d)), #dashed yellow line at mean
             color="yellow", linetype="dashed") +
  stat_function(fun = function(x) dnorm(x, mean = mean(oscars$birth_d), sd = sd(oscars$birth_d)) * length(oscars$birth_d) ,
                color = "purple", size = 1)


#quantile quantile plot
ggplot() + aes(sample=oscars$birth_d)+stat_qq()

#draw 1000 observations from a normal distribution, mean = 2, sd = 5
normal <- rnorm(1000,mean=2,sd=5)

#plot histogram
ggplot() + aes(normal) +
  geom_histogram(binwidth=1,color="black") +
  geom_vline(aes(xintercept=mean(normal)), #dashed yellow line at mean
             color="yellow", linetype="dashed") +
  stat_function(fun = function(x) dnorm(x, mean = mean(normal), sd = sd(normal)) * length(normal) ,
                color = "purple", size = 1)


#quantile quantile plot
ggplot() + aes(sample=normal)+stat_qq()
