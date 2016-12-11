setwd("J:/new data/store_data/soda")

soda <- read.csv("soda_county.csv")
demo <- read.csv("county_demo.csv")

library(dplyr)
# Mergeing by variable needs to be a charater
# to check how R is reading your variable, use the function "str"

str(demo)
str(soda)

# Notice that FIPS is being treated as a Numeric Integer.
#lets convert this to Character (Nominal) in both data

soda$FIPS=as.character(soda$FIPS)
demo$FIPS=as.character(demo$FIPS)

# LEFT JOIN MEANS KEEP EVERY THING FROM SODA FILE EVEN IF DEMO FOR COUNTIES IS MISSING

soda_demo= left_join(soda,demo) # Notice size of merged file is same as soda

# If we use "inner_join" it keeps only counties where both files have FIPS number
# TRY YOURSELF. 

# Save the merged file as CSV


##########################################################################################################################
# HOW TO SAVE A R DATA TO CSV
# NOTE THIS WILL BE SAVED IN YOUR WORKING DIRECTORY
##########################################################################################################################

write.csv(soda_demo, file="soda_demo.csv" )

