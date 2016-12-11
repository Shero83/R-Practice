#################################################################################################
# Statistcs Review ( & Preview of Techniques to come)
#################################################################################################

setwd("C:\\Users\\Sherif\\Google Drive\\NYU\\4 Fall 14\\D3M\\Assg 3")

retail <- read.csv("Internet_retail.csv")

#load necessary package
library(ggplot2)


# Try Quintiles of Income, this is creating a new variable "IncQuint" within the store_demo file
# "with" is used to tell R to compute this variable and add to the original file in 1 command
# if we wanted "Quartile" we would change 5 to 4 to divide our 2000 stores into 25% quartiles based on ZIP code income

retail$IncQuint<-with(retail, cut(retail$Total.life.to.date.dollars, quantile(retail$Total.life.to.date.dollars,(0:5)/5),include.lowest=TRUE))

# Go check the store_demo file. It should have a new variable in the last column called "INCQuint"
# Lets tell R that this is a "nominal Variable" and Add Labels to Quintile
retail$IncQuint <- factor(retail$IncQuint, labels = c ("Low Cust. Rev." ,"Quint2", "Quint3" ,"Quint4" ,"High Cust. Rev.") )

# Box plot of Cambell Share by  Income Quintiles
byIncQuint<-ggplot(retail, aes(x=IncQuint, y=House.credit.card., fill=IncQuint))+geom_boxplot()

plot(byIncQuint)

total.revenue = sum (retail$Total.life.to.date.dollars)
total.top.20 = by(retail$Total.life.to.date.dollars, retail$IncQuint, sum)
total.top.20[5]/total.revenue*100
# Notice how the distribution is shifting down meaning that shares of campbell shift down at higher income

# And in every region
byIncQuint+ facet_wrap(~REGION)

