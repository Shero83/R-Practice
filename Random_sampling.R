
setwd("C:\\Users\\Sherif\\Google Drive\\NYU\\4 Fall 14\\D3M\\R Data")

store_demo <- read.csv("store_demo.csv")


###############################################################################################
# Random Sampling
##############################################################################################

#check population means for couple of variables as check
mean(store_demo$capita_inc)
mean(store_demo$Share_Camp)

# Generate a random sample 

library(dplyr)

set.seed(3)
rnorm(1)

samp1<-sample_n(store_demo,500)

# Check means in your sample
mean(samp1$capita_inc)
mean(samp1$Share_Camp)

################################################################################################
# Divide stores into Test & Control Markets
###############################################################################################

# Create an Index to divide stores in 2 Groups
index = sample(1:nrow(store_demo), size=0.5*nrow(store_demo))

# Test & Control Stores 
test = store_demo[index,]
control =store_demo[-index,]

# Check means in test & control markets

mean(test$capita_inc)
mean(control$capita_inc)

mean(test$Share_Camp)
mean(control$Share_Camp)

# Add a column to test & control
test["type"]<-"test"
control["type"]<-"control"

# Stack both data into file (on top of eachother)
both<-rbind_list(test, control)

 
# Shares of Campbell by REGION as box plot
ggplot(both, aes(x=type, y=Share_Camp, fill=type))+geom_boxplot()
ggplot(both, aes(x=type, y=capita_inc, fill=type))+geom_boxplot()

summarise(group_by(both, type),mean=mean(Share_Camp), sd=sd(Share_Camp))
