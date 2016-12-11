######################################################################################################################
# Analysis of 2012 Presidential Election: County Data (Part 1)
# Summary Statistics
# D3M, NYU Stern
#####################################################################################################################

# Set Your working directory and load data

setwd("C:/Users/Sherif/Google Drive/NYU/4 Fall 14/DDDM/R Data")

# Read data and give a temp name "election" 
election <- read.csv("county_election_2012.csv")


# Quick check on what variables have missing values
check1<-colSums(is.na(election))
print(check1)

# So some health variables e.g.  "Per__Fair_Poor"  & "Menatally_Unhealthy_Days" have lot of missing values
# What happens if we remove ALL row with missing value??

# Create a new data that removes all rows with any missing values-- for any variable
election1<-na.omit(election)
# Check the size of our data sets
dim(election)
dim(election1)

# Why do we loose so many observations??? NOT a Good idea to drop all rows. 
# This is important from a practical perspective. 

# Get summary of % Voting  
summary(election1$Per_Obama)
summary(election1$Per_Romney)

##########################################################################################################################
# INTRO TO "dplyr" 
# Get votes by States. Use a new package called dplyr
# This is brand new by Rstudio. Suppose to work with blazing speed for large data sets. 
#########################################################################################################################
install.packages("dplyr")
library(dplyr)
state.votes<-election %>%
  group_by(State) %>%
  summarise(state_total=sum(Num_Total_Vote), state_obama=sum(Num_Obama_Total), Per_obama_state=(state_obama/state_total))


head(state.votes) # This displays the first few data poits

# Suppoe we want only Colorado
colorado<-filter(state.votes, State=='Colorado')

###########################################################################################################################
# Plotting in R
# ggplot is the main package, has recently been extended to "ggvis" that allows you to get HTML plots
##########################################################################################################################


#load necessary package
install.packages("ggplot2")
library(ggplot2)

# Histogram of Obama %
ggplot(election, aes(x=Per_Obama)) + geom_histogram()

# Histogram by Region
byregion<-ggplot(election, aes(x=Per_Obama, fill=census_region)) + geom_density(alpha=.8)
plot(byregion)
# Get seperate graphs for each Region
byregion+ facet_wrap(~census_region)

# Correlations with other demographics, lets say Income
ggplot(election, aes(x=Per_Obama, y=Household_Income))+geom_point()+ stat_smooth(method=loess)


# Try Log Income
logincome<-log(election$Household_Income)
# Attach log income to data
election<-cbind(election, logincome)

# Try correlation with log income. Also switch x & y
ggplot(election, aes(x=logincome, y=Per_Obama))+geom_point()+ stat_smooth(method=loess)

# Try correlation with log income. Also switch x & y
tmp<-ggplot(election, aes(x=logincome, y=Per_Obama))+geom_point()

# See correlation by census region
tmp+ geom_smooth(aes(group=census_region), method="loess")+ facet_wrap(~census_region)

# See correlation by RED BLUE STATES
tmp+ geom_smooth(aes(group=Red_blue_state), method="loess")+ facet_wrap(~Red_blue_state)

# *********************************************** Assignment 1: Do below for Romney *********************************************
# Try correlation with log income. Also switch x & y
tmpR<-ggplot(election, aes(x=Income_capita, y=Per_Romney))+geom_point()

# See correlation by census region
tmpR+ geom_smooth(aes(group=census_region), method="loess")+ facet_wrap(~census_region)

# See correlation by RED BLUE STATES
tmpR+ geom_smooth(aes(group=Red_blue_state), method="loess")+ facet_wrap(~Red_blue_state)
# *********************************************** Assignment 1: Do below for Romney *********************************************

##################################################################################################
###It is often useful to transform a continous variable to discrete. 
# For example, we can divide the counties into quintiles of income (Lowest 20%, ...Highest 20%)
##################################################################################################

# Try Quintiles of Income, creates buckets of data to catch non-linearities:
election$IncQuint<-with(election, cut(election$Household_Income, quantile(election$Household_Income,(0:5)/5,na.rm=TRUE),include.lowest=TRUE))

# Add Labels to Quintile
election$IncQuint <- factor(election$IncQuint, labels = c ("Low Income" ,"Quint2", "Quint3" ,"Quint4" ,"High Income") )

summarise(group_by(election, IncQuint),mean=mean(Household_Income))
         
# Box plot of % Obama along Income Quintiles
ggplot(election, aes(x=IncQuint, y=Per_Obama, fill=IncQuint))+geom_boxplot()



##########################################################################################################################

# Check Correlations between Voting &  ALL Variables. 

#########################################################################################################################

# Select  some variables  .

list1<-c('Per_Obama',  'Per_Romney',   	'Per_Young',	'Per_65_and_over',	'Per_African_American',
         'Per_white',	'Rural', 'Percent_College_More',  'Income_capita',	'Household_Income')

#  Remove na values .
short_elec<- na.omit(election[c(list1)])

#check correlations
cor1<-cor(short_elec, use="pairwise.complete.obs")
cor1



# A good Package to visualize correlations 
install.packages("corrplot")
library(corrplot)

corrplot(cor1, method="shade")

corrplot(cor1, method="circle")

# Order your correlations
corrplot(cor1, order = "AOE", method="square")

# Correlations based on 3 clusters
corrplot(cor1, order = "hclust", addrect = 3)

# Correlations based on 3 clusters, use Squares (Blue is pos correlated, red is neg)
corrplot(cor1, order = "hclust", addrect = 3, method="square")

# Google corrplot to get various other formats

#############################################################################################################
# Create Quintiles for Best & Worst Counties for Obama & Romney
# Save the file and explore in Tableau
#############################################################################################################
# Quintiles of Obama Vote
election$ObamaQuint<-with(election, cut(election$Per_Obama, quantile(election$Per_Obama,(0:5)/5,na.rm=TRUE),include.lowest=TRUE))
# Add Labels to Quintile Obama
election$ObamaQuint <- factor(election$ObamaQuint, labels = c ("Worst Obama" ,"Quint2", "Quint3" ,"Quint4" ,"Best Obama") )

# Quintiles of Romney Vote
election$RomneyQuint<-with(election, cut(election$Per_Romney, quantile(election$Per_Romney,(0:5)/5,na.rm=TRUE),include.lowest=TRUE))
# Add Labels to Quintile Romney
election$RomneyQuint <- factor(election$RomneyQuint, labels = c ("Worst Romney" ,"Quint2", "Quint3" ,"Quint4" ,"Best Romney") )

# Save the file, lets claa it election_new.csv. 
# There are few extra columns now
write.csv(election, file="election_new.csv" )
