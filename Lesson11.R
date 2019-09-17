
#install.packages("ggplot2")
library(ggplot2)

#install.packages("broom")
#library(broom)


##################################
#Linear regression
##################################

#Load the tips.txt data set

tips <- read.delim("tips.txt")
names(tips)

#scatter plot # of people (x) vs. 
#bill (y)

ggplot(tips, aes(x=nPeop, y=bill)) + geom_point() +
  labs(title="Number of people vs. bill",
       x="Number of people", y = "Bill")

#add a regression line
ggplot(tips, aes(x=nPeop, y=bill)) +
  geom_point() +
  geom_smooth(method=lm) +
       labs(title="Number of people vs. bill",
            x="Number of people", y = "Bill")

#remove the confidence interval
ggplot(tips, aes(x=nPeop, y=bill)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE) +
  labs(title="Number of people vs. bill",
       x="Number of people", y = "Bill")

#Equation of regression line
# response ~ predictor
tipsReg <- lm(bill~nPeop,data=tips)
tipsReg
summary(tipsReg)

#plot the residuals
#data = fit + residual
tipsRegf <- fortify(tipsReg)
ggplot(tipsRegf, aes(x = nPeop, y = .resid)) + geom_point() +
  labs(title="Number of people vs. bill",
       x="Number of people", y = "Bill")

#residual qq plot
resid <- residuals(tipsReg)
ggplot() + aes(sample=resid)+stat_qq()

#predict pay given years experience
predict.lm(tipsReg,data.frame(nPeop=5))
predict.lm(tipsReg,data.frame(nPeop=1:10))
           
           
###########################################

#scatter plot # of people (x) vs. 
#tip (y)

ggplot(tips, aes(x=nPeop, y=tip)) + geom_point() +
  labs(title="Number of people vs. tip",
       x="Number of people", y = "tip")

#add a regression line
ggplot(tips, aes(x=nPeop, y=tip)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE) +
  labs(title="Number of people vs. tip",
       x="Number of people", y = "tip")

#Equation of regression line
tipsReg <- lm(tip~nPeop,data=tips)
tipsReg
summary(tipsReg)

#plot the residuals
tipsRegf <- fortify(tipsReg)
ggplot(tipsRegf, aes(x = nPeop, y = .resid)) + geom_point() +
  labs(title="Number of people vs. tip",
       x="Number of people", y = "tip")

#predict pay given years experience
predict.lm(tipsReg,data.frame(nPeop=5))
predict.lm(tipsReg,data.frame(nPeop=1:10))

###########################################
###########################################
###########################################

#Load the teacher.txt data set

teacher <- read.delim("teacher.txt")
names(teacher) #column names

#scatter plot years (x) vs. 
#base pay in thousands (y)

ggplot(teacher, aes(x=years, y=base/1000)) + geom_point() +
  labs(title="Years experience vs. base pay",
       x="Years", y = "Pay")


#add a regression line
ggplot(teacher, aes(x=years, y=base/1000)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE) +
  labs(title="Years experience vs. base pay",
       x="Years", y = "Pay")

#Equation of regression line
teacherReg <- lm(base/1000~years,data=teacher)
teacherReg
summary(teacherReg)

#plot the residuals
teacherRegf <- fortify(teacherReg)
ggplot(teacherRegf, aes(x = years, y = .resid)) + geom_point() +
  labs(title="Residuals: years experience vs. base pay",
       x="Years experience",y="Residuals")

#predict pay given years experience
predict.lm(teacherReg,data.frame(years=13))
predict.lm(teacherReg,data.frame(years=c(13,14,50)))


###########################################
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

ggplot(gifted, aes(x=cartoons, y=score)) + geom_point() +
  labs(x="Cartoons", y = "Gifted score")


#Equation of multiple regression line on all variables
giftedReg <- lm(score~.,data=gifted)
giftedReg
summary(giftedReg)

#pValues function returns p-values for each t-test
pValues <- function(fit){
  summary(fit)$coefficients[,4]
}
pVals <- pValues(giftedReg)
pVals

#create function to perform backward selection

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

#function to run backSelect with different alphas
#and pick model with largest adjusted R^2

backSelectBest <- function(data,from,to,by){
  result <- list()
  rSquaredAdj <- numeric()
  index <- 1
  for (i in seq(from,to,by)) {
    #invisible(capture.output(result[[index]] <- backSelect(data,i))) #run backSelect for each i but suppress pVals of each cycle
    result[[index]] <- backSelect(data,i)
    rSquaredAdj[index] <- summary(result[[index]])$adj.r.squared
    index <- index + 1
  }
  maxIndex<- which.max(rSquaredAdj) #index of max R squared
  return(result[[maxIndex]])
}


bestFit <- backSelectBest(gifted,.01,.15,.01)
summary(bestFit)$adj.r.squared


#try the county data set
county <- read.delim("county.txt")
names(county)
str(county)
is.factor(county)
countyNum <- county[,-c(1,2)]
#to predict poverty, rearrange columns so that poverty is first
data <- countyNum[,c(4,1:3,5:8)]
names(data)

backSelect(data,.05)
bestFit <- backSelectBest(data,.01,.15,.01)
summary(bestFit)$adj.r.squared



countyComplete <- read.delim("countyComplete.txt")
names(countyComplete)
str(countyComplete)
ccNum <- countyComplete[,-c(1,2)]
names(ccNum) #poverty is 31st column
#to predict poverty, rearrange columns so that poverty is first
data <- ccNum[,c(31,1:30,32:51)]
names(data)
dataNoNA <- data[complete.cases(data), ]
dim(dataNoNA) #removes most of the data
#remove columns with many NAs (related to firms)
data <- data[,-c(13,43,44,seq(37,42,1))]
dataNoNA <- data[complete.cases(data), ]
dim(dataNoNA)

backSelect(dataNoNA,.05)
bestFit <- backSelectBest(dataNoNA,.01,.15,.01)
summary(bestFit)$adj.r.squared



