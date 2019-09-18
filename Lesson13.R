
#install.packages("ggplot2")
library(ggplot2)



##################################
#ANOVA: Analysis of Variance
##################################

#data is here: http://www2.stat.duke.edu/~mc301/data/movies.html

movies <-  load("movies.RData") 
names(movies)

class(movies$critics_rating)
levels(movies$critics_rating)
summary(movies$critics_rating)

#hypothesis test: 
#H0: ave audience score is the same for each 
#level of critics_rating
#HA: at least one ave is different

#what is the population?

#F test where F = MSG/MSE 
#mean sq error between groups/ mean sq error within groups

#Assumptions: independence, normal distribution, similar variances
#(in all groups)

#Ingredients for the test:
data <- data.frame(cr = movies$critics_rating, as = movies$audience_score)

#Overall average
xBar = mean(data$as)

#average and sd for certified fresh
xBarCF = mean(data$as[data$cr == "Certified Fresh"])
sdCF = sd(data$as[data$cr == "Certified Fresh"])
#average and sd for fresh
xBarF = mean(data$as[data$cr == "Fresh"])
sdF = sd(data$as[data$cr == "Fresh"])
#average and sd for rotten
xBarR = mean(data$as[data$cr == "Rotten"])
sdR = sd(data$as[data$cr == "Rotten"])

xBar
xBarCF
xBarF
xBarR
sdCF
sdF
sdR

#Are variances about the same?

boxplot(data$as~data$cr)

#F = MSG/MSE (between/within variation)
#For MSG, divide by df = # levels - 1
#For MSE, divide by df = sample size - # levels

#Anova in R: aov()
moviesAnova <- aov(data$as~data$cr, data = data)
summary(moviesAnova)

#another data set

# http://www2.stat.duke.edu/~mc301/data/acs.html

acs <- load("acs.RData")
names(acs)

levels(acs$edu)

boxplot(acs$income~acs$edu)

#Anova for income by education level
acsAnova <- aov(acs$income~acs$edu, data = acs)
summary(acsAnova)

#what are the hypotheses?
#are assumptions satisfied?

#Anova for hrs_work by education level
boxplot(acs$hrs_work~acs$edu)
acsAnova <- aov(acs$hrs_work~acs$edu, data = acs)
summary(acsAnova)

#F probability density functions
ggplot(data.frame(x = c(0, 4)), aes(x)) + 
  stat_function(fun = df, args = list(df1 = 1,df2=1), aes(col='df=1,1')) +
  stat_function(fun = df, args = list(df1=3,df2=2), aes(col='df=3,2')) +
  stat_function(fun = df, args = list(df1=3,df2=8), aes(col='df=3,8')) +
  stat_function(fun = df, args = list(df1=6,df2=12), aes(col='df=6,12')) +
  stat_function(fun = df, args = list(df1=10,df2=20), aes(col='df=10,20')) +
  scale_colour_manual("Legend", values = c("df=1,1"="red", "df=3,2"="purple", "df=3,8"="blue","df=6,12"="green","df=10,20"="orange"))


###########################################
###########################################
######Nonparametric Tests##################
###########################################
###########################################


#################################
######Sign test##################
#################################

#Is critics median score 50?
#H0: critics median score is 50
#HA: critics median score is not 50 (two-tailed)

library(BSDA)

SIGN.test(movies$critics_score,md=50,alternative="two.sided")
SIGN.test(movies$critics_score,md=61,alternative="two.sided")

#################################
######Wilcoxon Signed Rank Test##
#################################

#for paired data
#populations from same distribution


#Load the gifted.txt data set

gifted <- read.delim("gifted.txt")
names(gifted) #column names

ggplot(gifted, aes(x=motheriq, y=fatheriq)) + geom_point()# +
 # labs(x="Read", y = "Gifted score")


#H0: paired difference has median 0
#HA: paired diff has median != 0
data <- gifted$motheriq - gifted$fatheriq
wilcox.test(gifted$motheriq,gifted$fatheriq,paired=TRUE,alternative="two.sided")


#################################
######Mann Whitney U Test########
#################################

#for unpaired data: do two populations have same distribution?
#H0: same distribution

length(movies$runtime[movies$genre=="Drama"])
length(movies$runtime[movies$genre=="Comedy"])


median(movies$runtime[movies$genre=="Drama"])
median(movies$runtime[movies$genre=="Comedy"])


wilcox.test(movies$runtime[movies$genre=="Drama"], movies$runtime[movies$genre=="Comedy"])
