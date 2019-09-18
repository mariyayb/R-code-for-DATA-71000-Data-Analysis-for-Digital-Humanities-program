
#install.packages("ggplot2")
library(ggplot2)

#open from NYC Open Data
# https://data.cityofnewyork.us/Recreation/-Kids-In-Motion-Playground-Programming/8p6c-94pc

#Attendance information for the "Kids in Motion" program, 
#which provides free activities in NYC’s playgrounds, 
#including organized sports, games, fitness demos, and board games.


##################################
#Difference of proportions
##################################

#We can assume the difference of
#two sample proportions 
# is approximately normally 
#distributed if
#1. Separately, each proportion is approx. normal 
#(SRS and success-failure condition)
#2. The two samples are independent of each other

#The same formulas apply, except 
#use for standard error:
#SE = sqrt(p1(1-p1)/n1 + p2(1-p2)/n2)

#Example: does phrasing of a question
#influence the response?

#As you may know, by 2014 nearly all Americans will 
#be required to have health insurance. 
#Version ??? while 
#Version ???.
#Do you approve or disapprove of this policy?

#Version A: "people who do 
#not buy it will pay a penalty"

#Version B: "people who 
#cannot afford it will receive 
#financial help from the government"

#Create the table

survey <- matrix(c(771, 47, 49, 3, 732, 34, 63, 3),ncol=4,byrow=TRUE)
colnames(survey) <- c("Sample size","Approve %","Disapprove %","Other %")
rownames(survey) <- c("A first","B first")
survey <- as.table(survey)
survey

#Create a 95% confidence interval
#for the difference in approval (proportion)

pA1 <- survey[1,2]/100
pB1 <- survey[2,2]/100
pDiff <- pA1 - pB1

nA1 <- survey[1,1]
nB1 <- survey[2,1]

z <- qnorm(0.95 + 0.05/2)

SE <- sqrt((pA1*(1-pA1))/nA1 + (pB1*(1-pB1))/nB1) 

#confidence interval
min <- pDiff - z*SE
max <- pDiff + z*SE
min
max

###Hypothesis test for difference of proportions
#H0: pA1 - pB1 = 0
#HA: pA1 - pB1 != 0
#alpha = 0.05

#z-score
z <- (pDiff - 0)/SE

#p-value
p <- 2*pnorm(abs(z),lower.tail=FALSE)

#Read more about survey methods here:
# https://www.pewresearch.org/methods/u-s-survey-research/questionnaire-design/

################################
###Chi squared tests
################################


#Chi square probability density functions
ggplot(data.frame(x = c(0, 25)), aes(x)) + 
  stat_function(fun = dchisq, args = list(df = 1), aes(col='df=01')) +
  stat_function(fun = dchisq, args = list(df=2), aes(col='df=02')) +
  stat_function(fun = dchisq, args = list(df=3), aes(col='df=03')) +
  stat_function(fun = dchisq, args = list(df=4), aes(col='df=04')) +
  stat_function(fun = dchisq, args = list(df=10), aes(col='df=10')) +
  scale_colour_manual("Legend", values = c("df=01"="red", "df=02"="purple", "df=03"="blue","df=04"="green","df=10"="orange"))

#alpha = 0.05

ggplot(data.frame(x = c(0, 25)), aes(x)) + 
  stat_function(fun = dchisq, args = list(df=2), aes(col='df=02')) +
  stat_function(fun = dchisq, args = list(df=2), xlim = c(qchisq(.95,df=2),25),
                geom = "area", fill = "purple", alpha = .5)+
  stat_function(fun = dchisq, args = list(df=4), aes(col='df=04')) +
  stat_function(fun = dchisq, args = list(df=4), xlim = c(qchisq(.95,df=4),25),
                geom = "area", fill = "green", alpha = .5)+
  stat_function(fun = dchisq, args = list(df=10), aes(col='df=10')) +
  stat_function(fun = dchisq, args = list(df=10), xlim = c(qchisq(.95,df=10),25),
                geom = "area", fill = "orange", alpha = .5)+
  scale_colour_manual("Legend", values = c("df=02"="purple", "df=04"="green","df=10"="orange"))



###################################
###Chi squared goodness of fit test
###################################

#Is the proportion of kids events 
#the same among all 5 boroughs?

#If so, we expect (1/5)*length(kids$Borough)
#events in each borough

kids <- read.csv("Kids_In_Motion.csv")
dim(kids)
names(kids)

borough <- kids$Borough
table <- table(borough) #table of counts of events for each borough

n<-50 #sample size
boroughSample <- sample(borough,n)
tableSample <-table(boroughSample)
tableSample

#we expect (1/5)*legnth(borough)
#events in each if proportion is the same
#H0: each borough has same proportion of events (1/5)
#HA: the proportion of events in at 
#least one borough is not 1/5
#alpha = 0.05
#Chi-square goodness -of - fit test
#The test statistics will be chi-square = 
#sum of (observed - expected)^2 / expected
#condition: expected count is at least 5 for each category

#expect, use rep() function to repeat 5 times
expected <- rep((1/5)*length(boroughSample),5)
expected

#convert tableSample to vector
observed <- as.vector(tableSample)
observed

#the test statistic

ChiSqr <- sum(((observed-expected)^2)/expected)
ChiSqr

#p-value
#df = length(expected) - 1

p <- pchisq(ChiSqr, df=4,lower.tail=FALSE)
p


#Test again, using the expected proportion to be
#the actual population proportions
#H0: proportion of events in each borough
#is the same as proportion of population
#HA: prop. of events in at least one 
#of the boroughs is not the same as prop. of total population
#alpha = 0.05
#df = 5 - 1 = 4

names(table(kids$Borough))
#population in millions
populationNYC <- c(1.471,2.649,1.665,2.359,0.479)
expected <- length(boroughSample)*populationNYC/sum(populationNYC)
expected

ChiSqr <- sum(((observed - expected)^2)/expected)
ChiSqr

#p-value (one-sided, right-tailed test)
p <- pchisq(ChiSqr,df=4,lower.tail=FALSE)
p



################################
###Chi squared independence test
################################


#Using: Fast Response Survey System (FRSS): Elementary School Arts Education Survey, Fall 2009 (ICPSR 36067)
# https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/36067/datadocumentation#
#CAUTION: file size 13 MB

#Arts data region vs. question: 
#Did your school sponsor 
#school performances or presentations
#during the 2008-09 school year?

load("~/Dropbox/CityTech/Teaching/DATA 71000/os3_data/ICPSR_36067/DS0001/36067-0001-Data.rda")

#create data fram from the four variables
arts <- data.frame("Region" = da36067.0001$OEREG, 
                   "Size" =  da36067.0001$SIZE,
                   "Urban" = da36067.0001$URBAN,
                   "Q18EA"=da36067.0001$Q18EA)

#save this data frame
save(arts,file="arts.Rda")
#to load it in the future
#load("arts.Rda")

artsTable <- table(arts$Region, arts$Q18EA)
artsTable

#Chi square test for independence
#H0: variables are independent
#HA: variables are dependent
#alpha = 0.05
#Note: this is a one-sided test

#Test Region vs Q answer
artsObserved <- as.matrix(artsTable)
artsObserved

#function IndepTest
IndepTest <- function(observed,i,j) {
  total <- sum(observed) #total sample size
  (rowSums(observed)[i]*colSums(observed)[j])/total
}

   
artsExp<-rep(NA, 4*2)
artsExpected <- matrix(artsExp,nrow=4,ncol=2)



for(i in 1:4) {
  for(j in 1:2) {
  artsExpected[i,j] <- IndepTest(artsObserved,i,j)  
  }
}  
artsExpected


#ChiSqr
ChiSqr <- sum(((artsObserved-artsExpected)^2)/artsExpected)
ChiSqr

#p value
p <- pchisq(ChiSqr, df=(4-1)*(2-1),lower.tail=FALSE)
p



chisq.test(arts$Region, arts$Q18EA)
chisq.test(arts$Urban, arts$Q18EA)
chisq.test(arts$Size, arts$Q18EA)

artsTable <- table(arts$Size, arts$Q18EA)
artsTable
