################################################################################################
# LOGIT ANALYSIS OF CDC Data 
# Random Sample of approx 1/2 million from pooled 2006-2010 BRFSS files
# D3M, NYU Stern
################################################################################################


setwd( "C:\\Users\\vsingh.NYC-STERN\\Dropbox\\teaching\\2014\Fall\\Lectures\\Logit\\" )

cdc<-read.csv('obesity_clean.csv')


####################################################################################################
# SUMMARY STATISTICS
###################################################################################################

# Obesity Rates: just take the mean of obese dummy
mean(cdc$obese)

# Check obesity by education 
aggregate(formula=obese~Education, data=cdc, FUN = mean)

# Check obesity by race
aggregate(formula=obese~Race, data=cdc, FUN = mean)

# Check obesity by education & race
aggregate(formula=obese~Education+Race, data=cdc, FUN = mean)



########################################################################################
# OLS to Understand the coefficients/data
#######################################################################################

# Start with bunch of OLS to understand coefficient. 
ols_model1<-lm(obese~ Education, data=cdc)
summary(ols_model1)


ols_model1<-lm(obese~ Race, data=cdc)
summary(ols_model1)

# Set base reference RACE to White
cdc <- within(cdc, Race <- relevel(Race, ref = "White non hispanic"))
ols_model1<-lm(obese~ Race, data=cdc)
summary(ols_model1)



#########################################################################################
# Run a basic LOGIT MODEL (MODEL 1)
#########################################################################################


# OLS Model 1
ols_model1<-lm(obese~ AGE+ Education+Income+Race+Gender+census_region, data=cdc)
summary(ols_model1)

# generalized model
#  logit model 1
logit_model1<-glm(obese~ AGE+ Education+Income+Race+Gender+census_region,family = binomial, data=cdc)
summary(logit_model1)


###########################################
# LOOK AT BOTH OLS & LOGIT RESULTS
############################################

library(texreg)

# Print on your r-studio screen
screenreg(list(ols_model1, logit_model1))

# Lets put estimates and s.e in 1 row using "single.row command"
screenreg(list(ols_model1, logit_model1), single.row=TRUE)

# If you want to SAVE the output as a text file to copy/past in word/ppt documents
screenreg(list(ols_model1, logit_model1), single.row=TRUE, file="run1.txt")

# SAVE at HTML
htmlreg(list(ols_model1, logit_model1), single.row=TRUE, file="run1.htm")

############################################
# LOGIT Coefficients are not directly interpretable
# Compute "MARGINAL EFFECTS": Holding all other variables constant, change in Obesity probability by changing the variable
# NOTE Regression directly gives you the marginal effects
############################################

# Compute marginal Effects for LOGIT Model 1
Marginal_Logit1 <- mean(dlogis(predict(logit_model1, type = "link"))) 
Marginal_Logit1<-Marginal_Logit1 * coef(logit_model1)

# Put both OLS & Logit Model Marginal Effects
Marginal_Both<- cbind(ols_model1$coef, Marginal_Logit1)
print(Marginal_Both)


############################################################################################
#MODEL 2: USE AGE AS CATEGORIES RATHER THAN CONTINOUS
#Call this MODEL 2
############################################################################################

# Use Age Groups rather then continous age--this is lower case "age"
cdc<- transform(cdc, agegroup=cut (AGE, c(0, 24, 40, 55, 70, 100), labels=c("Young","25-40","41 to 55","55 to 70","70+")))

ols_model2<-lm(obese~ agegroup+ Education+Income+Race+Gender+census_region, data=cdc)
summary(ols_model2)


#  logit model with age groups
logit_model2<-glm(obese~ agegroup+ Education+Income+Race+Gender+census_region,family = binomial, data=cdc)
summary(logit_model2)

screenreg(list(ols_model2, logit_model2), single.row=TRUE)

# Compute marginal Effects for LOGIT Model 1
Marginal_Logit2 <- mean(dlogis(predict(logit_model2, type = "link"))) 
Marginal_Logit2<-Marginal_Logit2 * coef(logit_model2)

# Put both OLS & Logit Model Marginal Effects
Marginal_Both<- cbind(ols_model2$coef, Marginal_Logit2)
print(Marginal_Both)



###################################################################################################
#MODEL COMPARISONS 
##################################################################################################

# Which is a better model? Compare AGE is  continous (model1) or discrete (model2)
# Lower values of AIC & BIC indicate better Model
# both cases Model2 outperforms Model 1. Keep Model 2 for futre analysis

AIC (logit_model1 , logit_model2)
BIC (logit_model1 , logit_model2)



#########################################################################################
# Predicted probabilities. 
# Get predicted probabilies and save the file to study in Tableau
#########################################################################################

#Logit Model 1
plogit1<- predict(logit_model1, type="response")
summary(plogit1)

#Logit Model 2
plogit2<- predict(logit_model2, type="response")
summary(plogit2)

# Also do OLS prediction for comparison\
# Linear regression models
pols1<- predict(ols_model1)
summary(pols1)
pols2<- predict(ols_model2)
summary(pols2)


# Attach prediction to original data 
cdc$plogit1=plogit1
cdc$plogit2=plogit2

cdc$pols1=pols1
cdc$pols2=pols2


#####################################################################################
# Save File for analysis in Tableau
####################################################################################

write.csv(cdc, file="cdc_pred.csv" )


