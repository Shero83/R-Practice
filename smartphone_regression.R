# Set your Work directory & read data

setwd ("C:/Users/vsingh.NYC-STERN/Dropbox/teaching/2014/Fall/Data/")
mydata<- read.csv("smart_phone_short.csv", header = TRUE)

# Convert your dependent variable to numeric. RHS variables can be non-numeric 

# "ifelse" works like Excel, create a new variable "smart_ph"

mydata$smart_ph <- ifelse(mydata$smart_phone=="Yes",1,0)
# new variable "smart_ph" is 0/1 corresponding to "No/Yes"

# Lets create Income Quintiles 
mydata$Income_Quint<-with(mydata, cut(mydata$income,quantile(mydata$income,(0:5)/5,na.rm=TRUE),include.lowest=TRUE))
# Add Labels to Quintile
mydata$Income_Quint <- factor(mydata$Income_Quint, labels = c ("Low Income" ,"Quint2", "Quint3" ,"Quint4" ,"High Income") )

# Run 3 Regressions

#Only web Visits
web<-lm(smart_ph~Facebook+LinkedIn+NYTimes.com+WSJ.com, data=mydata)
# Only Demographics
demo<-lm(smart_ph~Education+generation+Income_Quint, data=mydata)
# Both demographics and Web visits
demo_web<-lm(smart_ph~Education+generation+Income_Quint+Facebook+LinkedIn+NYTimes.com+WSJ.com, data=mydata)
  
library(texreg)

screenreg(list( web, demo, demo_web), single.row=TRUE, custom.model.names= c("WEB" , "DEMO" , "FULL MODEL"))

#save output to your working directory (if you need to take it to Excel or paste in report--not required for exam)
screenreg(list( web, demo, demo_web), single.row=TRUE, custom.model.names= c("WEB" , "DEMO" , "FULL MODEL"), file="smart_phone.txt")
