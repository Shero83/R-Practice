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

#################################################################################################################
# Model Comparison: For later
############################################################################################################
