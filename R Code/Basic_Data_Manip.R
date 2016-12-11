#####################################################################################################
# This code provides basic data manipulation in R using "dplyr" package
####################################################################################################

# We will use a simplified version of election data in file "dplyr_election.csv"

# Set your working directory where file is located. 

setwd("C:/Users/vsingh.NYC-STERN/Dropbox/teaching/2014/Fall/R Resources")

election<- read.csv("dplyr_election.csv", header = TRUE)

##############################################################################################
# Load the 'dplyr' package.
# if you get an error, first install the package, then run library command
###############################################################################################


library(dplyr)

# See what is in the data and how R is reading each variable
# 3 types, Integer, Factor, or Numbers
str(election)

# another way of looking at it if you have 'dplyr' loaded up
glimpse(election)

##################################
# Exercise1: select()
# We want to keep only a few variables (columns) from our data
##################################

# create new dta election1 with only State, Per_Obama, and Per_unemployed
# FIRST entry after select is the name of the Original FILE, followed by names of variables you want

election1<- select(election, State, Per_Obama, Per_unemployed)

# Check the file election1

# Suppose we want all columns from State to Per_Romney. Use ":"
election1<- select(election, State:Per_Romney)

# Check the file election1. This time it has 8 columns

# Same as above but we also want Household_Income which is in the end
election1<- select(election, State:Per_Romney, Household_Income)



##################################
# Exercise 2: filter()
# Now we want to keep only certain rows
# For example, data for only NY
##################################

# Keep only counties in NY
election1 <- filter(election, State=="New York")

# check the file election1

# Keep only NY counties that have more than 50,000 Total votes casted

election1 <- filter(election, State=="New York" & Num_Total_Vote>50000)

# Only 22 counties in NY have Total votes > 50,000



##################################
# Exercise 3: arrange()
# This is to sort your data
##################################

# Sort by Per_Romney
election1 <- arrange(election,  Per_Romney)

head(election1) # see first few rows, lowest Per_Romney are on top
tail(election1) # Print Last few rows

# Sort from High to low for Romney %
election1 <- arrange(election,  desc(Per_Romney))

head(election1)


##################################
# Exercise 4: mutate()
# This is to Create New Variables
##################################

# Create a new variable, log income 
election1 <- mutate(election, logincome= log(Household_Income))

# See election1 file, last column has logincome

# What % of poluation is not in the labor force, e.g kids, in school, retired, etc.
# Lets call the variable "Not_working"

election1 <- mutate(election, Not_working= Num_Labor_Force/Num_Population)
head(election1)

                    
##################################
# Exercise 5: summarise()
# Get Summary Statistics 
##################################


summarise(election1, avg_not_working = mean(Not_working))

# same thing without label
summarise(election1,  mean(Not_working))



##################################
# Exercise 6: group_by()
# This is powerful command that allows for "BY" statement
##################################

# Get % Obama votes by State
election1<- summarise(group_by(election, State), Obame.ByState = sum(Num_Obama_Total)/sum(Num_Total_Vote))

# See the file election1, it has all states and % Obama votes 
head(election1)

# Lets Get Obama votes by Census Regions 
election1<- summarise(group_by(election, census_region), Obame.ByRegion = sum(Num_Obama_Total)/sum(Num_Total_Vote))
head(election1)

##################################
# Exercise 7: MERGING DATA SETS
##################################

# Lets first create 2 data sets from election file

data1<- select(election, FIPS, State, Per_Obama, Per_unemployed)
head(data1)

# another data with 2 variables 
data2<-select(election, FIPS, census_region)
head(data2)

# data1 and data2 have one common variable "FIPS" 
# We can merge 2 data sets based on FIPS code

both_data<- merge(data1, data2, by="FIPS")

##########################################################################################################################
#Exercise 8:  HOW TO SAVE A R DATA TO CSV
# NOTE THIS WILL BE SAVED IN YOUR WORKING DIRECTORY
##########################################################################################################################

# Saving election1 file to CSV file called "election_junk.csv"

write.csv(election1, file="election_junk.csv" )

# Your working directory shold have election_junk.csv 