#################################################################################
# Clustering based on Google trends
# D3M NYU Stern
################################################################################

setwd( "C:\\Users\\vsingh.NYC-STERN\\Dropbox\\teaching\\2014\\Fall\\Data\\" )

g1<-read.csv('Google_example.csv')

# Leave out state name
g2<-g1[,2:24]

# Scale the variables
g_scale=scale(g2)


# QUick look at correlations in searches
library(corrplot)

check_corr <- cor (g2)

corrplot(check_corr, method="shade")

# Correlations based on clusters
corrplot(check_corr, order = "hclust", addrect = 4, method="square")

# Do a quick clustering of states and map it
# To reproduce results set seed. K- mean has a random component to it.
set.seed(1)

CLUSTER <- kmeans(g_scale, 5, nstart=20)


#see segment sizes
CLUSTER$size

# Save the file with cluster memberships attached and gfo to Tableau

g1<- cbind(g1, CLUSTER$cluster)

#Save the final File in working directory for analysis in Tableau
write.csv(g1, file = "google_seg1.csv")
