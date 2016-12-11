#################################################################################################
# Factor & Cluster Analysis
# Example 1: Clutering Stores in the Soup Data
# D3M, NYU Stern
#################################################################################################

setwd("C:/Users/vsingh.NYC-STERN/Dropbox/teaching/2014/Fall/Lectures/factor")

store_demo <- read.csv("store_demo.csv")

# Our data has approx 2000 stores located across the country. Look at the data to remind yourself

# lets stack all are demographics to a new file "demo"
demo<-store_demo[, 9:18]


############################################################################
# FACTOR ANALYSIS
# Think of this as summary analysis of correlations of your variables
# In most large datasets that you will see, many of the variables will be highly correlated
############################################################################

# Lets start with a Check on correlations in all our demographic variables

cor1<- cor(demo)

#  visualize correlations 

library(corrplot)

corrplot(cor1, method="shade")

# With numbers
corrplot.mixed(cor1)

# Correlations based clusters
corrplot(cor1, order = "hclust", addrect = 4, method="square")



#################################################################################################################
# Principle Component or Factor Analysis is a way to reduce the number of correlated variables into 
# a smaller set of underlying "factors" or "themes". Think of these as combining higly correlated variables
# to a new variable. There are many packages avaialable in R to do this. Lets use "psych"
#############################################################################################################

#load necessary package

library("psych")

# Guidance on how many underlying "factors" there are in your data. 
# See what gets printed on the console

HowMany_Fac <-fa.parallel(demo)

# Conduct Factor Analysis: Based on above, all demographics are capturing 3 basic "constructs/themes/ideas"

FA <- factanal(demo, 3, scores="regression")

# "FA" is the name we gave to store output.
# Now we will have 3 new varaibles that capture the "essence" of all 10 demographics

#Lets Interpret what the 3 new variables  mean. 
#Only show correlations bigger than .5 for ease of interpretation

print(FA, cutoff=.5)

# INTERPRETATION OF FACTORS
# Factor 1 is capturing "families with kids"
# Factor 2 is socio-economic
# Factor 3 is race

# The row "Cumulative Var" telss us that 3 Factors capture about 78% of information in 10 original demograpics 

# So Instead of using 10 demographics which are highly correlated, we can use 3 Factors

# The 3 new variables that are created such that 
# (1) They are standardized (mean=0, std=1) 
# (2) are completely uncorrelated with each other
 
# Lets examine the 3 new variables. 

#see various output in FA
names(FA)

# One component of that output is "score". This is where the new variables are stored.

summary(FA$scores)

# Check Correlation of new variables 
cor_scores<-cor(FA$scores)

corrplot(cor_scores, method="shade")

# Attach Factor Scores to Data
store_demo<- cbind(store_demo, FA$scores)

# Check the last 3 columns of store_demo file, these are the new variables

###################################################################
# Lets see how we can use these Factor scores
# Regression of Progresso shares
##################################################################

# Suppose we want to run a regression of Progresso shares on demographics 

# Use the orignial demographics 
reg1<- lm(Share_prog~ per_kids18  +per_kids9	+ per_old	+ per_white	+ per_black	+
            hhsize	+ Per_college	+ per_hh_less20K	+ capita_inc, data=store_demo)
summary(reg1)

# Now lets use the new variables. We get close enough R-sq 
# and this model is just using 3 variables. 
reg2<- lm(Share_prog~Factor1+Factor2+Factor3, data=store_demo)
summary(reg2)

###################################################################################
# MARKET SEGMenTATION: CLUSTER ANALYSIS
###################################################################################

# Segmentation of stores based on Factors & Market Share

# Combine Factor "Scores" & Market Shares into a new file, call the file "segment"
# Note we are pulling out market shares & newly generated Factor Scores for segementation

segment<-store_demo[, 19:25]

# See the file to understand what variables we are using

# To reproduce results set seed. K- mean has a random component to it.
# run the line below so we all get the same answer
set.seed(5001)

CLUSTER <- kmeans(segment, 5, nstart=20)

# We are segmenting the stores in 5 groups. "nstart" says run the analysis 20 times 
# from different starting places for robustness check

# Main outputs to see 
# (1) see segment sizes

CLUSTER$size

# (2) see segment interpretation. This will be better in Tableau
CLUSTER$centers

# append cluster assignment to original data set 
store_final<- cbind(store_demo, CLUSTER$cluster)

# see the last column in data. It will have a number 1 to 5 which tells us which segment each sotre belogs to

#Save the final File in working directory for analysis in Tableau
write.csv(store_final, file = "store_final.csv")

#For Q5
cor2<- cor(store_final$median_inc, store_final$Factor2)
cor2
