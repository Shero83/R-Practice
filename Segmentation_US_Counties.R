
# Set Your working directory and load data

setwd("C:/Users/vsingh.NYC-STERN/Dropbox/teaching/2014/Fall/Lectures/factor")

LE <- read.csv("county_segment1.csv", header=TRUE)

# Segment based on % variables. 
#Leaving out highly correlated variables measuring population (Totoal votes, population etc.)
# Scale your variables and store in a file called "demo"

demo<-scale(LE[,11:34])

#NEED TO RUN THIS

set.seed(5001) 

# So our answers match. 

mycluster <- kmeans(demo, 5, nstart=50, iter.max=1000)

 
#see segment sizes

mycluster$size

# append cluster assignment to original data set 
LE_final<- cbind(LE, mycluster$cluster)

#Save the final File in working directory for analysis in Tableau
write.csv(LE_final, file = "County_cluster.csv")

