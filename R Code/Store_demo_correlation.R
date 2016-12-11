#################################################################################################
# Statistcs Review ( & Preview of Techniques to come)
#################################################################################################

setwd("C:\\Users\\Sherif\\Google Drive\\NYU\\4 Fall 14\\D3M\\R Data")

store_demo <- read.csv("store_demo.csv")

# Lets take the perspective of progbell

# Always start by summarizing data. What is MY Market share?
summary(store_demo$Share_prog)


# How else should we summarize a varible? 
# Our data has approx 2000 stores located across the country
# Its useful to see the full distribution of shares

#load necessary package
library(ggplot2)


# ****************FOR LIVE GRAPHS USE ggvis, you can have live data on the graphs ******************

# Histogram of progbell Share

ggplot(store_demo, aes(x=Share_prog)) + geom_histogram()

# Histogram by Region, store it in "byregion". We can then add to this 
byregion<-ggplot(store_demo, aes(x=Share_prog, fill=REGION)) + geom_density(alpha=.4)
plot(byregion)

# Get seperate graphs for each Region
byregion+ facet_wrap(~REGION)


# Box plot is another way of looking at the distribution. 
# Shares of progbell by REGION as box plot
ggplot(store_demo, aes(x=REGION, y=Share_prog, fill=REGION))+geom_boxplot()

# progbell clearly does better in Midwest & South


##############################################################################################
# Check correlation with other variables
##############################################################################################

# Correlations with other demographics, lets say Income
ggplot(store_demo, aes(x=median_inc, y=Share_prog))+geom_point()+ stat_smooth(method=lm)


#########################################################################################
# NOTICE that some Some ZIP codes are outliers, very high income compared to others
# A useful tranformation is Log in these cases
# Create a new variable log income and attach it back to data
#############################################################################################

# *************************************** log pushes outliers in ***************************************
logincome<-log(store_demo$median_inc)

# Attach log income to data
store_demo<-cbind(store_demo, logincome)

# Try correlation with log income. Store it in tmp
tmp<-ggplot(store_demo, aes(x=logincome, y=Share_prog))+geom_point()+ stat_smooth(method=lm)

plot(tmp)

# NEXT See regression by census region

tmp+ geom_smooth(aes(group=REGION), method="lm")+ facet_wrap(~REGION)

# All regions look similar in direction (high income--> lower shares for progbell)
# Seems like the line is steeper in South. What does that mean?

##################################################################################################
#It is often useful to transform a continous variable to discrete. 
# For example, we can divide the counties into quintiles of income (Lowest 20%, ...Highest 20%)
##################################################################################################

# Try Quintiles of Income, this is creating a new variable "IncQuint" within the store_demo file
# "with" is used to tell R to compute this variable and add to the original file in 1 command
# if we wanted "Quartile" we would change 5 to 4 to divide our 2000 stores into 25% quartiles based on ZIP code income

store_demo$IncQuint<-with(store_demo, cut(store_demo$median_inc, quantile(store_demo$median_inc,(0:5)/5),include.lowest=TRUE))

# Go check the store_demo file. It should have a new variable in the last column called "INCQuint"
# Lets tell R that this is a "nominal Variable" and Add Labels to Quintile
store_demo$IncQuint <- factor(store_demo$IncQuint, labels = c ("Low Income" ,"Quint2", "Quint3" ,"Quint4" ,"High Income") )

# Box plot of Cambell Share by  Income Quintiles
byIncQuint<-ggplot(store_demo, aes(x=IncQuint, y=Share_prog, fill=IncQuint))+geom_boxplot()

plot(byIncQuint)

# Notice how the distribution is shifting down meaning that shares of progbell shift down at higher income

# And in every region
byIncQuint+ facet_wrap(~REGION)


##########################################################################################################
# WHAT HAVE WE LEARNT SO FAR? 
# progBELL Shares vary by regions
# Shares also Seem to be correlated with income, higher income people don't like progbell
##########################################################################################################




##########################################################################################################################

# Check Correlations with other Variables. 
# We should At least check correlation with shares of other brands
# What Can simple correlations teach us?

#########################################################################################################################



##########################################################################################################
# Lets create a data with ALL continous variables in the data
# We can't do correlations with nominal variables such as "REGION" or "IncQuintile"
# In this dtaa all continous variables are in rows 9 to 22
# From "per_kids18" to "share_prog" (Lets ignore logincome in column 23)
###########################################################################################################


#check correlations for all continuous variables (columns 9 to 22) and store it in "cor1"

cor1<-cor(store_demo[,9:22])


# A good Package to visualize correlations 
# Google "corrplot" to learn more

library(corrplot)

corrplot(cor1, method="shade")

corrplot(cor1, method="circle")

# Lets visualize and see the actual numbers as well
corrplot(cor1, method='number')


# Order your correlations
corrplot(cor1, order = "AOE", method="square")

# Correlations based on 3 clusters
corrplot(cor1, order = "hclust", addrect = 3, method="square")


# Correlations based on 4 clusters
# DO IT YOURSELF BY COPY/PASTE ABOVE AND MAKING NECESSARY CHANGES


############################################################################################################
# What can we learn from these correlations? 

# Lets see correlations b/w market shares and demographics seperately
############################################################################################################

# Market share are in columns 19 to 22. Either type innames, (eg "Share_prog") or use matrix to say
# give me all rows (i.e. all stores) but only columns 19 to 22 and store their correlations in "cor_of_share"

cor_of_share<-cor(store_demo[,19:22])

# Lets visualize and see the actual numbers as well
corrplot.mixed(cor_of_share, lower='number', upper='square')

# Another version of the same thing
corrplot.mixed(cor_of_share, lower='ellipse', upper='number')

# JUST DEMOGRAPHICS, they are in rows 9 to 18
cor_of_demos<-cor(store_demo[,9:18])


# Correlations based on 4 clusters
corrplot(cor_of_demos, order = "hclust", addrect = 4, method="square")

######################################################################################################

########################################################################################################################

# Another useful approach to analyze variables that are correlated (and provide similar information)
# maybe to group them using clustering. This is somewhat different than how we think of clustering
# which is typically performed on "rows". For example cluster the counties or stroes. We will see this later
# A useful package for this is "clustofvar". Lets see what it gives
# Thius package can handle both QUANTITATIVE & NOMINAL Variables 
#######################################################################################################################

install.packages("ClustOfVar")

library("ClustOfVar")

X.quanti<-store_demo[,9:22] # Lets first just check all numerical variables. 

tree <- hclustvar(X.quanti) 

plot(tree)

