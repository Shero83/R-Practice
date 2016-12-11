# Set Your working directory and load data

setwd("/Users/leyahfarber/Google Drive/MBA2/D3M/Assignments/Assignment 2 Due Sept 23/")

# Read data and give a temp name "LE" 
LE <- read.csv("FEMALE_LE.csv", header=TRUE, sep=",")

# Quick check on what variables have missing values
colSums(is.na(LE))

# Several health variables have missing values
# What happens if we remove ALL rows with missing value?
LE1<-na.omit(LE)
dim(LE)
dim(LE1)

# So we loose about 300 counties. How much does our key variable of interest? 
summary(LE$Female_life_Growth)
summary(LE1$Female_life_Growth)

# So we loose about 300 counties. How much does our key variable of interest? 
summary(LE$Femalelifeexpectancyyears_2010)
summary(LE1$Femalelifeexpectancyyears_2010)

#load necessary package
install.packages("ggplot2")
library(ggplot2)

# Correlations of Female Life Growth with County Median income
ggplot(LE1, aes(x=Rate_median_income, y=Female_life_Growth))+geom_point()+ stat_smooth(method=loess)

#Temporary file
tmp<-ggplot(LE1, aes(x=Rate_median_income, y=Female_life_Growth))+geom_point()+ stat_smooth(method=loess)
tmp+geom_smooth(aes(group=Red_blue), method="loess")+ facet_wrap(~Red_blue)

# Correlations of Female Life Growth with White
ggplot(LE, aes(x=Per_white, y=Female_life_Growth))+geom_point()+ stat_smooth(method=loess)

# Correlations of Female Life Growth with Black
ggplot(LE, aes(x=Per_black, y=Female_life_Growth))+geom_point()+ stat_smooth(method=loess)

# Correlations of Female Life Growth with Fast Food Per Capita
ggplot(LE, aes(x=Rate_Fast_Food_Restaurant, y=Female_life_Growth))+geom_point()+ stat_smooth(method=loess)

# Correlations of Female Life Growth with Limited Access to Healthy Food
ggplot(LE, aes(x=Per_Limited_Access_Healthy_Food, y=Female_life_Growth))+geom_point()+ stat_smooth(method=loess)

# Correlations of Female Life Growth with Divorce
ggplot(LE, aes(x=Percent_divorced, y=Female_life_Growth))+geom_point()+ stat_smooth(method=loess)

# Correlations of Female Life Growth with Religious Freaks
ggplot(LE, aes(x=Rate_Adeherence_Religion_All, y=Female_life_Growth))+geom_point()+ stat_smooth(method=loess)

# A good Package to visualize correlations 
install.packages("corrplot")
library(corrplot)

# Select  some variables  .

list1<-c('Rate_Adeherence_Religion_All',  'Percent_divorced',     'Per_Limited_Access_Healthy_Food',
         'Rate_Fast_Food_Restaurant',	'Per_black', 'Per_white', 'Female_life_Growth', 'Rate_median_income')

#  Remove na values .
short_LE2<- na.omit(LE[c(list1)])

#check correlations
cor1<-cor(short_LE2, use="pairwise.complete.obs")
cor1

# Order your correlations
corrplot(cor1, order = "AOE", method="square")

# Correlations based on 3 clusters
corrplot(cor1, order = "hclust", addrect = 3)

# Correlations based on 3 clusters, use Squares
corrplot(cor1, order = "hclust", addrect = 3, method="square")

