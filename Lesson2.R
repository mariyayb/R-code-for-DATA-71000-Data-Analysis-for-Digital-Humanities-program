#install.packages("ggplot2")
library(ggplot2)

#load email data set
email_data <- read.csv("email50.csv")

#####################
#Scatter Plots
#####################

#scatter plot of line breaks vs. number of characters
ggplot(email_data, aes(x=line_breaks, y=num_char)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

#scatter plot of line breaks vs. number of characters, separated by number variable
ggplot(email_data, aes(x=line_breaks, y=num_char, color=number)) + geom_point(shape=1)

#load county data set
county<-read.delim("county.txt")

#scatter plot of home ownership vs. poverty level
ggplot(county, aes(x=poverty, y=homeownership)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)  +# Don't add shaded confidence region
  ggtitle("Poverty vs. Home Ownership by County") + #title
  ylab("Home Ownership") +
  xlab("Poverty")


#equation of linear regression line
line <- lm(homeownership~poverty,data=county)
line


#scatter plot of home ownership vs. poverty level
ggplot(county, aes(x=poverty, y=homeownership, color=med_income)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region


#####################
#Dot Plots
#####################

#Dot plot of num_char in the email data set (experiment with binwidth
#to see what changes)
ggplot(email_data, aes(x = num_char)) + geom_dotplot(binwidth=1) +
  scale_y_continuous(NULL, breaks = NULL) 

ave_num_char <- mean(email_data$num_char) #average of num_char stored as ave_num_char
ave_num_char #to see the value of ave_num_char


#####################
#Histogram
#####################


#histogram of num_char in the email data set (experiment with binwidth)
ggplot(data=email_data, aes(x=num_char)) +
  geom_histogram(binwidth=10,color="black") +
   geom_vline(aes(xintercept=mean(num_char)), #dashed yellow line at mean
              color="yellow", linetype="dashed")


#same histogram, with color coded bars according to value of "number"
#from the email data set
ggplot(data=email_data, aes(x=num_char,color=number)) +
  geom_histogram(binwidth=15)
  
#experiment with binwidth; to stop push the red stop button in the console
ggplot(data=email_data, aes(x=num_char,color=number)) +
  geom_histogram(binwidth=.001)

#####################
#Box and whisker plot
#####################

#access teacher.txt data and store in variable "teacher"
teacher<-read.delim("teacher.txt")
class(teacher) #this is called a data fram in R

#box and whisker plot of years column
ggplot(data=teacher, aes(y=years)) +
  geom_boxplot()

#box and whisker plot of years column for each degree type
ggplot(data=teacher, aes(y=years,x=degree)) +
  geom_boxplot()

#box and whisker plot of base column for each degree type
ggplot(data=teacher, aes(y=base,x=degree)) +
  geom_boxplot()

#box and whisker plot of base column for each degree type
ggplot(data=teacher, aes(y=base,x=degree)) +
  geom_boxplot() +
    stat_summary(fun.y=mean, geom="point",color="red") #add red dot where the mean lies

