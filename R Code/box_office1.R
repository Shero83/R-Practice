################################################################################
# Introduction to Regression in R
# Predicting Box Office Revenue
###############################################################################

library(ggplot2)


boxoffice <- read.csv("Box_Office.csv")

# Run a Linear Regression 
Reg_wrong<-lm(Opening_Week_Revenue~Genre+Num_Theaters+Overall_Rating, data=boxoffice)
names(Reg_wrong)

# R is treating Genre as a continous variable
summary(Reg_wrong)


#RECODE GENRE VARIABLES OBSERVATIONS INTO THEIR CATEGORICAL NAMES: factor sets the variable as a dimension rather than a measure
genrenames<-factor(boxoffice$Genre, 
                   levels=1:4, 
                   labels=c("action", "comedy", "kids", "other"))

#COMBINE THE CATEGORICAL NAME DATA FRAME WITH THE BOX OFFICE DATA 
boxoffice=data.frame(boxoffice, genrenames)


# check the distribution of key variables 
ggplot(boxoffice, aes(x=Opening_Week_Revenue))+geom_histogram (binwidth=5)

# Add Color by Genre of Moview
ggplot(boxoffice, aes(x=Opening_Week_Revenue, fill=genrenames))+geom_histogram ()

# Density Plot
ggplot(boxoffice, aes(x=Opening_Week_Revenue, fill=genrenames))+geom_density ()

# Box plot maybe be better to see
ggplot(boxoffice, aes(x=genrenames, y=Opening_Week_Revenue, fill=genrenames))+geom_boxplot()

########################################################################################
# All plots show there are outliers, some movies make a lot of money
# Log transformation may be useful to try
########################################################################################


# Lets first try raw correlation b/w continous variables
cor(boxoffice[,c(2:4)])

# Scatter Plot of Revenue and Theater
ggplot(boxoffice, aes(x=Opening_Week_Revenue, y=Num_Theaters))+geom_point()+stat_smooth()

# DO A SCATTER PLOT of REVENUE & Rating


# Check Means by Genre 
aggregate(formula=Opening_Week_Revenue~genrenames,data=boxoffice, FUN = mean)


#RUN THE REGRESSION (GENRE ONLY)
reg<-lm(Opening_Week_Revenue~genrenames, data=boxoffice)
summary(reg)


# Run same regression but Use "Comedy" as the base

boxoffice <- within(boxoffice, genrenames <- relevel(genrenames, ref = "comedy"))
reg1<-lm(Opening_Week_Revenue~genrenames, data=boxoffice)
summary(reg1)

##################################################################################################
# To print clean tables in your R console, lets use "texreg" package"
# Install it first
################################################################################################

library(texreg)

screenreg(list(reg, reg1))


#################################################################################################
# Linear Regression with ALL Variables
###############################################################################################
library(coefplot)
#RUN LINEAR REGRESSION
Linregfull<-lm(Opening_Week_Revenue~genrenames+Num_Theaters+Overall_Rating, data=boxoffice)

summary(Linregfull)
screenreg(list(reg1, Linregfull))


# Run a LOG BASED MODEL. 

#Take Log of Revenue, Theater, & Rating

boxoffice$logRev<-log(boxoffice$Opening_Week_Revenue)
boxoffice$logTheater<-log(boxoffice$Num_Theater)
boxoffice$logRating<-log(boxoffice$Overall_Rating)

# Run log regression

Logreg<-lm(logRev~genrenames+logTheater+logRating, data=boxoffice)

summary(Logreg)
screenreg(list(Logreg))


# >>> Lower AIC is better <<<
AIC(reg1)
AIC(Linregfull)
AIC(Logreg)


