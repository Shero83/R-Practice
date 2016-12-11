############################################################################################
# Brand Mining
# D3M, NYU Stern
###########################################################################################


setwd("C:/Users/vsingh.NYC-STERN/Dropbox/teaching/2014/Fall/Lectures/factor")

# Read the data
bav = read.csv(file = "BAV_FACTOR1.csv", header = TRUE)
dim(bav)

# The attributes are located in columns 16 to 63
# We could use "Total Users & Total Pref" as well but start with attributes only
# Conduct the analysis by category or everything? Lets start with ALL INDUSTRIES

att1<-bav[, 16:63]


# We will use the "psych" package for the analysis
# Google "R Psych" or see help menu for more information

install.packages("psych")
library("psych")


#plot correlation matrix
corr1<-cor(att1)

# Here we have so many attributes that even visualizing simple correlations is difficult

library(corrplot)

corrplot(corr1, order = "hclust", addrect = 4, method="square")

# Reduce the size of labels using "t1.cex".Change this based on what you want 
corrplot(corr1, order = "hclust", addrect = 4, method="square", tl.cex = .5)

# Not much help, basically seems like bunch of attributes are higly correlated
#Save the final File in working directory for analysis in Tableau
write.csv(corr1, file = "bav_corr.csv")

###################################################################################################
# This part of the code uses "psych" Package for Factor Analysis. 
# Start with PCA/Factor analysis 
#################################################################################################

# Guidance on Number of Factors
NUMFactors<-fa.parallel(att1)

#run factor analysis. Varimax rotation produces uncorrelated factors
FA1<- factanal(att1, 9, scores="regression")

#see various output in FA1
names(FA1)

#view factor analysis results
print(FA1, cutoff=.5)

# Easier to see Loadings in Excel for interpretation
#Save the factor loadings in CSV file
write.csv(FA1$loadings, file = "bav_loadings1.csv")

# Now we have new set of variables (Factor scores) that are completely uncorrelated
cor_FA1<-cor(FA1$scores)
corrplot(cor_FA1, method="shade")
round(cor_FA1,2)
# ADD THE NEW VARIABLES "Facotrs" to data
bav<-cbind (bav, FA1$scores)


# Run a regression of "Brand Asset" on Factors
BA_Reg<-lm(A_Brand_Asset_R~Factor1+Factor2+Factor3+Factor4+Factor5+Factor6+Factor7+Factor8+Factor9, data=bav)
summary(BA_Reg)

###################################################################################################
# Do segmentation of Brands Using CLUSTER ANALYSIS
# Lets to a 5-segment solution. Add Total Users to Segmentation
#################################################################################################

# Combine Factor Scores & "Users", call the file "segment"

segment<-cbind(bav$Total_Users_pct, FA1$scores)

CLUSTER <- kmeans(segment, 6, nstart=20)

#Check segment sizes
CLUSTER$size


# append Factor Scores & cluster assignment to original data frame
bav_final<- data.frame(bav, CLUSTER$cluster)

#Save the final File in working directory for analysis in Tableau
write.csv(bav_final, file = "bav_final.csv")
