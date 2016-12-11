#########################################################################################
# Segment NY ZIP COdes
# D3M, NYU Stern
########################################################################################

setwd("C:/Users/vsingh.NYC-STERN/Dropbox/teaching/2014/Fall/Lectures/factor")

NY <- read.csv("NY_Zip.csv")

# Remove Missing values
NY1<-na.omit(NY)

# Subet only numeric Values
NY2<- NY1[, 9:32]

# Standardize the variables--works better for clustering
NY2<-scale(NY2)

# To reproduce results set seed. K- mean has a random component to it.

set.seed(5001)

# Second number is how many segments you want
CLUSTER <- kmeans(NY2, 6, nstart=20)

# See the Output 
names(CLUSTER)

#see segment sizes
CLUSTER$size

#see segment interpretation--better to do in Tableau
CLUSTER$centers

# Attach to original data
NY_Seg<-cbind(NY1, CLUSTER$cluster)


#Save the final File in working directory for analysis in Tableau
write.csv(NY_Seg, file = "NY_Segment.csv")
