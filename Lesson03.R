#install.packages("ggplot2")
library(ggplot2)

#load email data set
email_data <- read.csv("email50.csv")

#####################
#Contingency Tables
#####################

#Create table for email data spam/not spam vs. number

email_data$spam <- as.factor(email_data$spam) #convert spam column to a factor (categorical variable in R)

email_table <- table(email_data$spam,email_data$number) #create a table of spam vs number column
rownames(email_table) <- c("Not spam","Spam") #set row names
email_table #display the table


columns <-colSums(email_table)

email_table <- rbind(email_table, columns)
email_table

rows <- rowSums(email_table)
email_table <- cbind(email_table,rows)
email_table

#####################
#Promotion Simulation
#####################

#promotion decisions, 1=promoted, 0=not promoted
decision <- c(rep(1,35),rep(0,13))

#permute decision 100 times with each permutation in column
random<-sapply(1:1000,function(x){
  sample(decision)})

#transpose
random <-t(random)

#suppose first 24 apps are male, next 24 are female
#count number of male promotions vs female promotions
#and take difference of proportions
differences<- (rowSums(random[,1:24])-rowSums(random[,25:48]))/24
differences<-data.frame(differences)

#plot histogram of differences
ggplot(differences, aes(x=differences)) +
  geom_histogram(bins=10,color="black")

#how many of the differences are equal to 0
length(differences[differences==0])

#how many of the differences are less than 0 
length(differences[differences<0])

#how many of the differences are greater/equal to 0.292
length(differences[differences>=0.292])

#how many of the differences are greater/equal to 0.292 
# or less than/equal to -0.292
length(differences[differences>=0.292 | differences<=-0.292])
