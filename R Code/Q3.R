setwd("C:\\Users\\Sherif\\Google Drive\\NYU\\4 Fall 14\\D3M\\Assg 3")

sample1 <- read.csv("sample1.csv")
sample2 <- read.csv("sample2.csv")

mydata <- merge(sample1, sample2, by=c("FIPS"))
mydata_all <- merge(sample1, sample2, by=c("FIPS"), all=TRUE)

by(mydata$FemaleLE, mydata$census_region, mean)
by(mydata_all$FemaleLE, mydata_all$census_region, mean)
