# Set Your working directory and load data

setwd("C:/Users/vsingh.NYC-STERN/Dropbox/teaching/2014/Fall/Assignments/Assignment 1")

# Read data and give a temp name "election" 
bing <- read.csv("bing_it_on1.csv", header=TRUE, sep=",")

library(gmodels)
CrossTable(bing$Search_Type, bing$Preference, chisq=TRUE,  format="SPSS")




#####################################################################################
# Other way to do this in base R
###################################################################################

#  Frequency Table & Chi-square test
mytab1 <- xtabs(~Search_Type+ Preference, data=bing)
ftable(mytab1) # print table 
summary(mytab1) # chi-square test of indepedence


