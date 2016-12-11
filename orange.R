
library(texreg) # For clean look at output


# Set your working directory where file is located. 


# Read Data 

oj <- read.csv("oj_competition.csv", header = TRUE)

# Create log of Sales & Price for each brand 

oj$log_trop_sales= log(oj$TropSales)
oj$log_florida_sales= log(oj$FolridaSales)
oj$log_MM_sales= log(oj$MMSales)
oj$log_PL_sales= log(oj$PLSales)

# Create Log prices for all brands

oj$log_price_trop<-log(oj$PriceTrop)
oj$log_price_florida<-log(oj$PriceFlorida)
oj$log_price_MM<-log(oj$PriceMM)
oj$log_price_PL<-log(oj$PricePL)


# NOTE Useful to see the data. It now has sales/price of all brands in log 

# We have to run 4 regressions, one for each brand.
# dependent variable is log-sales for each brand & predictor are all 4 prices (in log)

#Tropicana Regression
reg_trop<-lm(log_trop_sales~log_price_trop+log_price_florida+log_price_MM+log_price_PL ,data=oj)
summary(reg_trop)


# Florida Regression
reg_florida<-lm(log_florida_sales~log_price_trop+log_price_florida+log_price_MM+log_price_PL ,data=oj)
summary(reg_florida)


# MM Regression 
reg_MM<-lm(log_MM_sales~log_price_trop+log_price_florida+log_price_MM+log_price_PL ,data=oj)
summary(reg_MM)

# Private Lable Regression
reg_PL<-lm(log_PL_sales~log_price_trop+log_price_florida+log_price_MM+log_price_PL ,data=oj)
summary(reg_PL)


# See regression output for ALL brands

screenreg(list(reg_trop, reg_florida, reg_MM, reg_PL))

# Understand what "coef" command does
coef(reg_trop)
# so it just prints out regression coefficients for Tropicana regression without std. error, t-stat etc. 

# Lets stack coefficients from all regression to create an "elasticity matrix" 

Elasticity_Matrix<-rbind(coef(reg_trop), coef(reg_florida), coef(reg_MM), coef(reg_PL))

# Print the Price Elasticity Matrix
print(Elasticity_Matrix)

# save the elastitcity matrix & understant it in Excel.

write.csv(Elasticity_Matrix, file="OJ_reg_output.csv" )

# Your working directory should have file 'oj_reg_output.csv". Use this to answer questions
