
#install.packages("ggplot2")
library(ggplot2)

#install.packages("Rmisc")
library(Rmisc)

##################################
#Linear regression examples
##################################

#Some artificial data sets for illustration

########################
###Example 1 ###########
########################

predictor <- seq(1,50,1) #integers 1 through 50
response <- 0.5*predictor + 3
Data <- data.frame(predictor,response)

#scatter plot # of (x) vs. (y)
#with regression line


ggplot(Data, aes(x=predictor, y=response)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE)
  
#Equation of regression line
# response ~ predictor
dataReg <- lm(response~predictor,data=Data)
dataReg
summary(dataReg)

#plot the residuals
#data = fit + residual
dataRegf <- fortify(dataReg)
ggplot(dataRegf, aes(x = predictor, y = .resid)) + geom_point() 

#residual qq plot
resid <- residuals(dataReg)
ggplot() + aes(sample=resid)+stat_qq()


########################
###Example 2 ###########
########################

predictor <- seq(1,50,1) #integers 1 through 50
response <- 0.5*predictor + 3 + rnorm(50)
Data <- data.frame(predictor,response)

#scatter plot # of (x) vs. (y)
#with regression line

p0 <- ggplot(Data, aes(x=predictor, y=response)) +
  geom_point()
  
  
p1 <- ggplot(Data, aes(x=predictor, y=response)) +
    geom_point() +
    geom_smooth(method=lm,se=FALSE, fullrange=TRUE)

#plot the residuals
#data = fit + residual
dataRegf <- fortify(dataReg)
p2 <- ggplot(dataRegf, aes(x = predictor, y = .resid)) + geom_point() 


#residual qq plot
resid <- residuals(dataReg)
p3 <- ggplot() + aes(sample=resid)+stat_qq()

#plot
multiplot(p0,p2,p1,p3, cols=2)

#Equation of regression line
# response ~ predictor
dataReg <- lm(response~predictor,data=Data)
dataReg
summary(dataReg)

########################
###Example 3 ###########
########################

predictor <- seq(1,50,1) #integers 1 through 50
response <- 0.5*predictor + 3 + c(rnorm(49),25)
Data <- data.frame(predictor,response)

#scatter plot # of (x) vs. (y)
#with regression line

p0 <- ggplot(Data, aes(x=predictor, y=response)) +
  geom_point()


p1 <- ggplot(Data, aes(x=predictor, y=response)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE)

#plot the residuals
#data = fit + residual
dataRegf <- fortify(dataReg)
p2 <- ggplot(dataRegf, aes(x = predictor, y = .resid)) + geom_point() 


#residual qq plot
resid <- residuals(dataReg)
p3 <- ggplot() + aes(sample=resid)+stat_qq()

#plot
multiplot(p0,p2,p1,p3, cols=2)

#Equation of regression line
# response ~ predictor
dataReg <- lm(response~predictor,data=Data)
dataReg
summary(dataReg)


########################
###Example 4 ###########
########################

predictor <- seq(1,50,1) #integers 1 through 50
response <- 0.5*predictor + 3 + c(rnorm(35,0,.5),rnorm(15,2,6))
Data <- data.frame(predictor,response)

#scatter plot # of (x) vs. (y)
#with regression line

p0 <- ggplot(Data, aes(x=predictor, y=response)) +
  geom_point()


p1 <- ggplot(Data, aes(x=predictor, y=response)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE)

#plot the residuals
#data = fit + residual
dataRegf <- fortify(dataReg)
p2 <- ggplot(dataRegf, aes(x = predictor, y = .resid)) + geom_point() 


#residual qq plot
resid <- residuals(dataReg)
p3 <- ggplot() + aes(sample=resid)+stat_qq()

#plot
multiplot(p0,p2,p1,p3, cols=2)

#Equation of regression line
# response ~ predictor
dataReg <- lm(response~predictor,data=Data)
dataReg
summary(dataReg)

########################
###Example 5 ###########
########################

predictor <- seq(1,50,1) #integers 1 through 50
response <- 0.01*predictor^2 + 3 #+ c(rnorm(35,0,.5),rnorm(15,2,6))
Data <- data.frame(predictor,response)

#scatter plot # of (x) vs. (y)
#with regression line

p0 <- ggplot(Data, aes(x=predictor, y=response)) +
  geom_point()


p1 <- ggplot(Data, aes(x=predictor, y=response)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE)

#plot the residuals
#data = fit + residual
dataRegf <- fortify(dataReg)
p2 <- ggplot(dataRegf, aes(x = predictor, y = .resid)) + geom_point() 


#residual qq plot
resid <- residuals(dataReg)
p3 <- ggplot() + aes(sample=resid)+stat_qq()

#plot
multiplot(p0,p2,p1,p3, cols=2)

#Equation of regression line
# response ~ predictor
dataReg <- lm(response~predictor,data=Data)
dataReg
summary(dataReg)


###########################################
###########################################
######Multiple Regression##################
###########################################
###########################################


#Load the gifted.txt data set

gifted <- read.delim("gifted.txt")
names(gifted) #column names

#scatter plot fatheriq (x) vs. 
#score (y)

ggplot(gifted, aes(x=fatheriq, y=score)) + geom_point() +
  labs(x="Father's IQ", y = "Gifted score")

#add a regression line
ggplot(gifted, aes(x=fatheriq, y=score)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE) +
  labs(x="Father's IQ", y = "Gifted score")

#Equation of regression line
giftedRegF <- lm(score~fatheriq,data=gifted)
giftedRegF
summary(giftedRegF)


#scatter plot motheriq (x) vs. 
#score (y)

ggplot(gifted, aes(x=motheriq, y=score)) + geom_point() +
  labs(x="Mother's IQ", y = "Gifted score")


#add a regression line
ggplot(gifted, aes(x=motheriq, y=score)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE) +
  labs(x="Mother's IQ", y = "Gifted score")

#Equation of regression line
giftedRegM <- lm(score~motheriq,data=gifted)
giftedRegM
summary(giftedRegM)

###########################################
###Multiple Regression
###########################################

names(gifted)

#some more scatter plots

ggplot(gifted, aes(x=read, y=score)) + geom_point() +
  labs(x="Read", y = "Gifted score")

ggplot(gifted, aes(x=edutv, y=score)) + geom_point() +
  labs(x="Educational TV", y = "Gifted score")


ggplot(gifted, aes(x=speak, y=score)) + geom_point() +
  labs(x="Speak", y = "Gifted score")

#age in months at which
#child counted to 10
ggplot(gifted, aes(x=count, y=score)) + geom_point() +
  labs(x="Count", y = "Gifted score")


#Equation of multiple regression line
giftedReg <- lm(score~motheriq+fatheriq+speak+edutv+count+read,data=gifted)
giftedReg
summary(giftedReg)

#make a preduction using newdata
newdata <- data.frame(motheriq = 120,fatheriq = 110, 
                      speak = 30, edutv = 5, count = 30, read = 3)

predict.lm(giftedReg,newdata)

#pValues function returns p-values for each t-test
pValues <- function(fit){
  summary(fit)$coefficients[,4]
}
pVals <- pValues(giftedReg)
pVals


#plot the residuals
#data = fit + residual
giftedReg <- fortify(giftedReg)
ggplot(giftedReg, aes(x = .fitted, y = .resid)) + geom_point() 




#create function to 
#perform backward selection
#based on p-values

#data is data frame with all variables, alpha is significance threshold
backSelect <- function(data,alpha) {
  #linear regression, response variable should be in first column
  fit <- lm(data[,1]~.,data[,-1])
  pVals <- pValues(fit)
  #if max p value is >= alpha and data has > 2 columns, 
  #proceed with removing variables
  while (max(pVals) >= alpha & dim(data)[2] > 2) { 
    maxIndex <- which.max(pVals)
    data <- data[,-maxIndex]
    
    fit <- lm(data[,1]~.,data[,-1])
    pVals <- pValues(fit)
  } 
  fit <- lm(data[,1]~.,data[,-1])
  pVals <- pValues(fit)
  print(pVals)
  return(fit)
}

backSelect(gifted,.1)
backSelect(gifted,.01)
backSelect(gifted,.05)

#######################################
###Logistic Regression#################
#######################################
email <- read.csv("email.csv")
names(email)

#plot of sigmoid function
#1/(1+e^(-x))
stats::plogis
ggplot() + stat_function(aes(-7:7), fun = plogis) + 
  xlab("logit(p)") + ylab("p")


#spam column as factor (categorical)
email$spam <- factor(email$spam)

logistic <- glm(spam ~ to_multiple, 
             data = email, family = "binomial")
logistic
summary(logistic)
#confidence intervals for coefficients
confint(logistic)

newdata <- data.frame(to_multiple = 1)
predict(logistic, newdata, type="response")


logistic <- glm(spam ~ to_multiple+winner+ format+
                  re_subj+exclaim_subj+cc+attach+dollar+
                  inherit+password,
                data = email, family = "binomial")
logistic
summary(logistic)
#confidence intervals for coefficients
confint(logistic)

newdata <- data.frame(to_multiple = 1,winner ="yes", format=1,
                        re_subj=1, exclaim_subj=1, cc=1, attach=1, dollar=1,
                        inherit=1,password=1 )
predict(logistic, newdata, type="response")

#p-values

#constant model
1-pchisq(logistic$null.deviance,logistic$df.null)
#fitted model
1-pchisq(logistic$deviance,logistic$df.residual)



logistic <- glm(spam ~ to_multiple+winner+ format+
                  re_subj+attach+password,
                data = email, family = "binomial")
logistic
summary(logistic)
#confidence intervals for coefficients
confint(logistic)

newdata <- data.frame(to_multiple = 1,winner ="yes", format=1,
                      re_subj=1, exclaim_subj=1, cc=1, attach=1, dollar=1,
                      inherit=1,password=1 )
predict(logistic, newdata, type="response")

#p-values

#constant model
1-pchisq(logistic$null.deviance,logistic$df.null)
#fitted model
1-pchisq(logistic$deviance,logistic$df.residual)

