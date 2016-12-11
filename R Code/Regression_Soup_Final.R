#############################################################################################
# Counter Cyclical Pricing
# D3M, NY Stern
############################################################################################

# Set Your working directory and load data
setwd("C:/Users/vsingh.NYC-STERN/Dropbox/teaching/2014/Fall/case studies/Counter Cyclical Pricing")

# Read data and put in your work space, lets call the file "soup"
soup <- read.csv("soup_new.csv", header=TRUE, sep=",")

# Check how R is reading your different variabes. Use "str" function

str(soup)



###################################################################################################################
# LINEAR  REGRESSION
#################################################################################################################

# Load library text reg for clean printing of results
library(texreg)


# LINEAR REGRESSION WITH OWN & Competitor Prices. Lets store the output in "Lin_m1" 

Lin_m1<- lm(Volume.Progresso~ Price.Progresso + Price.Campbell+ Price.PL+ Price.Other, data=soup)
summary(Lin_m1)

# Same as Model 1 above + Add Winter Dummy. Call it "Lin_m2"

Lin_m2<- lm(Volume.Progresso~ Price.Progresso + Price.Campbell+ Price.PL+ Price.Other+ Winter, data=soup)
summary(Lin_m2)

# Same as Model 2 above+ Add Region dummy

Lin_m3<- lm(Volume.Progresso~ Price.Progresso + Price.Campbell+ Price.PL+ Price.Other+ Winter + Region, data=soup)
summary(Lin_m3)

# See All 3 Models in same Table 
screenreg(list(Lin_m1, Lin_m2, Lin_m3))

###################################################################################################################
# LOG  REGRESSION. NOTE  natural LOG is "LOG" in R
#################################################################################################################

# Create New Column in data for Log Volume of Progresso

soup$log_prog_vol<-log(soup$Volume.Progresso)

# Create Log Price for all brands

soup$log_price_prog<-log(soup$Price.Progresso)
soup$log_price_camp<- log(soup$Price.Campbell)
soup$log_price_PL<-log(soup$Price.PL)
soup$log_price_other<-log(soup$Price.Other)

# Log REGRESSION WITH OWN & Competitor Prices. Lets store the output in "Log_m1" 

Log_m1<- lm(log_prog_vol~ log_price_prog+ log_price_camp+log_price_PL+ log_price_other, data=soup)
summary(Log_m1)

# Add Winter Dummy 
Log_m2<- lm(log_prog_vol~ log_price_prog+ log_price_camp+log_price_PL+ log_price_other+ Winter, data=soup)
summary(Log_m2)

# Add Region 
Log_m3<- lm(log_prog_vol~ log_price_prog+ log_price_camp+log_price_PL+ log_price_other+ Winter+ Region, data=soup)
summary(Log_m3)

# See All 3 Models in same Table 
screenreg(list(Log_m1, Log_m2, Log_m3))



##########################################################################################################

# MODEL COMPARISON: R-square is not always comparable across models. 

# USE AIC/BIC. LOWER VALUES OF BOTH IS BETTER

# AIC: Alkaike Infomation Criterion
# BIC: Bayesian Information Criterion

# MODEL WITH LOWER AIC/BIC are better
#########################################################################################################

# Compare Lin_M3 & Log_M3
#  ************************ CHECK WHAT THIS DOES ************************
AIC(Lin_m3, Log_m3)
BIC (Lin_m3, Log_m3)


####################################################################################################
EXTENSION # 1: ADD DUMMY VARIABLES FOR EACH MONTH RATHER THAN "WINTER"

########################################################################################################
# Extending the Models
# Maybe we can try adding Fixed Effects (i.e. Dummy variables) for each month
# So 11 dummy variables for Jan to Dec
#####################################################################################################

# First check how R is reading your "Dates"

str(soup)

#  Now Lets Tell R that "Date" is actually a calender date and not some "string/text/nominal" variable

soup$Date<-as.Date(soup$Date, "%m/%d/%Y" )

# Our data is monthly an in format month/day/year. %m/%d/%Y is telling R this formt
# Glance through (http://www.statmethods.net/input/dates.html) 

# Now its easy to create a new variable for "Months"

soup$month_dummy<-months(soup$Date)

library(ggplot2)
# Lets do a quick plot to see the month_dummy variable. Fill by our definition of "winter"
ggplot(soup, aes(x=month_dummy, y=log_prog_vol, fill=Winter))+geom_boxplot ()

# will be better if this sorted from Jan to dec. 
soup$month_dummy = factor(soup$month_dummy, levels = month.name)

# Now do the same plot & also seperate it out for census regions
ggplot(soup, aes(x=month_dummy, y=log_prog_vol, fill=Winter))+geom_boxplot ()+ facet_wrap(~Region)


# Extend Models 3 for Linear and Log regression by replacing "winter" with "month_dummy"

Lin_m4<- lm(Volume.Progresso~ Price.Progresso + Price.Campbell+ Price.PL+ Price.Other+ month_dummy + Region, data=soup)

# See model3 vs. model4  
screenreg(list(Lin_m3, Lin_m4))

# Model Comparison
AIC(Lin_m3, Lin_m4)

# DO the same thing with LOG models
Log_m4<- lm(log_prog_vol~ log_price_prog+ log_price_camp+log_price_PL+ log_price_other+ month_dummy+ Region, data=soup)
screenreg(list(Log_m3, Log_m4))

# MODEL WITH MONTH DUMMY IS BETTER THAN "WINTER" DUMMY
AIC(Log_m3, Log_m4)


#################################################################################################################
#Extension # 2 # Merge with Store Demographics
############################################################################################################

# Load Store Demographic Data

store_demo <- read.csv("store_demo.csv", header=TRUE, sep=",")

##################################################################################################
#It is often useful to transform a continous variable to discrete. 
# For example, we can divide the stores into quintiles of income (Lowest 20%, ...Highest 20%)
##################################################################################################

store_demo$IncQuint<-with(store_demo, cut(store_demo$median_inc, quantile(store_demo$median_inc,(0:5)/5),include.lowest=TRUE))

# Lets tell R that this is a "nominal Variable" and Add Labels to Quintile

store_demo$IncQuint <- factor(store_demo$IncQuint, labels = c ("Low Income" ,"Quint2", "Quint3" ,"Quint4" ,"High Income") )

# Lets also do Log Income
store_demo$LogIncome=log(store_demo$median_inc)

# KEEP ONLY IRI_KEY, State, Zip, median_income, LogIncome, & Quintile income we just created

library(dplyr)

store_demo_short = select(store_demo, IRI_KEY, State_Name, Zip_key, median_inc, LogIncome, IncQuint)

# Merge with "soup" file

soup_with_demo= merge(soup, store_demo_short, by="IRI_KEY")

# Regression with median income
Med_Inc<- lm(log_prog_vol~ log_price_prog+ log_price_camp+log_price_PL+ log_price_other
            + month_dummy+ Region+median_inc, data=soup_with_demo)


# Regression with logincome
Log_Inc<- lm(log_prog_vol~ log_price_prog+ log_price_camp+log_price_PL+ log_price_other
            + month_dummy+ Region+LogIncome, data=soup_with_demo)


# Regression with Income Quintile 
Inc_Q<- lm(log_prog_vol~ log_price_prog+ log_price_camp+log_price_PL+ log_price_other
             + month_dummy+ Region+IncQuint, data=soup_with_demo)

# See models with different ways of entering Income into regression

screenreg(list(Med_Inc, Log_Inc, Inc_Q))

# Even though R-square is not that different, AIC/BIC suggest Income Quintile may be the best way to enter Income in regression
AIC(Med_Inc, Log_Inc, Inc_Q)
BIC(Med_Inc, Log_Inc, Inc_Q)

####################################################################################################
# What do we have so far?
# (1) We started by doing simple Linear Models
# (2) Log Models work better in terms of standard model comparisons
# (3) We merged store demographics and compared 3 ways of adding Income to Model
###################################################################################################

# FINAL MODELS FOR LINEAR & LOG. SAVE PREDICTED VALUES

Lin_Fin<- lm(Volume.Progresso~ Price.Progresso + Price.Campbell+ Price.PL+ Price.Other+ month_dummy + Region +IncQuint, data=soup_with_demo)


# FINAL LOG MODEL  
Log_Fin<- lm(log_prog_vol~ log_price_prog+ log_price_camp+log_price_PL+ log_price_other + month_dummy+ Region+IncQuint, data=soup_with_demo)

AIC(Lin_Fin, Log_Fin)

####################################################################################################
#Add predicted sales to Data from your best Linear & Log Regression 
########################################################################################################


# In R, your regression output contains a variety of information such as coefficients, predicted values, residuals and so on

# Use "names" to see what all is contained in any regression output. For example, lets see "Log_m3" model
names(Lin_Fin)

# "fitted.values" is the predicted value from the model for each Store/Month (Fitted values -> predicted values)

# Add Predited Value from Linear Model "Lin_Fin", 

soup_with_demo$Predict_Linearmodel= Lin_Fin$fitted.values 

#Do the same for Log Model
# Note that because our dependent variable (Progresso.volume) is in LOG, prediction are also in LOG. 
# Lets use "exp" to get actual values and attach to data "soup_with_demo'

soup_with_demo$Predict_Logmodel= exp(Log_Fin$fitted.values) #check last column of your data

##################################################################################################################
# Finally, Lets save this file and explore in Tableau
##################################################################################################################

write.csv(soup_with_demo, file="soup_with_predicted.csv" )

# Your working directory should have a file called "soup_with_predicted.csv" 

