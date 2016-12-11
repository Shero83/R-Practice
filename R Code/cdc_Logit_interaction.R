################################################################################################
# CDC Data 
# UNDERSTANDING "INTERACTIONS"
################################################################################################


setwd( "C:\\Users\\vsingh.NYC-STERN\\Dropbox\\teaching\\2014\\Fall\\Lectures\\Logit\\" )

cdc<-read.csv('obesity_clean.csv', header=TRUE)

# Create a new variable "Agegroup" 
# Use Age Groups rather then continous age
cdc<- transform(cdc, agegroup=cut (AGE, c(0, 24, 40, 55, 70, 100), labels=c("Young","25-40","41 to 55","55 to 70","70+")))

library(texreg)


#########################################################################################
# Start with Simple Regression
#########################################################################################


# Simple regression of Obesity on Education
ols_model1<-lm(obese~  Education , data=cdc)
summary(ols_model1)

# Add Gender ot the Model
ols_model2<-lm(obese~  Education + Gender, data=cdc)
summary(ols_model2)

# Allow for "Interaction". Impact of Education is different for Males & Females
ols_model3<-lm(obese~  Education + Gender+ Education*Gender, data=cdc)
summary(ols_model3)

screenreg(list(ols_model1, ols_model2, ols_model3), single.row=TRUE)

# Use AIC or BIC for Model Comparison
AIC(ols_model1, ols_model2, ols_model3)


##############################################################################
# Understanding Interactions. 

# Suppose we subset our data for Males & Females and Run 2 seperate regressions

# USe "dplyr" package to subset data

library(dplyr)

# Females only data
Female_only<-filter(cdc,Gender=="Female")

# Female Regression
ols_fem<-lm(obese~  Education , data=Female_only)


# Males only data
Male_only<-filter(cdc,Gender=="Male")

#Male Regression
ols_male<-lm(obese~  Education , data=Male_only)


screenreg(list(ols_fem, ols_male, ols_model3), single.row=TRUE)


screenreg(list(ols_fem, ols_male, ols_model3), single.row=TRUE, custom.model.names= c("Female" , "Male" , "Joint"))


#########################################################################################
# Run  LOGIT MODEL 
#########################################################################################

#  logit model 3
logit_model3<-glm(obese~  Education + Gender+ Education*Gender, family = binomial, data=cdc)
summary(logit_model3)


###########################################
# LOOK AT BOTH OLS & LOGIT RESULTS
############################################


# Print on your r-studio screen
screenreg(list(ols_model3, logit_model3), single.row=TRUE)



############################################
# LOGIT Coefficients are not directly interpretable
# Compute "MARGINAL EFFECTS": Holding all other variables constant, change in Obesity probability by changing the variable
# NOTE Regression directly gives you the marginal effects
############################################

# Compute marginal Effects for LOGIT Model 1
Marginal_Logit3 <- mean(dlogis(predict(logit_model3, type = "link"))) 
Marginal_Logit3<-Marginal_Logit3 * coef(logit_model3)

# Put both OLS & Logit Model Marginal Effects
Marginal_Both<- cbind(ols_model3$coef, Marginal_Logit3)
print(Marginal_Both)


##################################################################################################
# Run 3 Models
# Model 1: Main effects of ALL Predictors with AGE as Continous
# Model 2: Same as model 1 but treat AGE as categories
# Modlel 3: Same as 2 But ALLOw for Interaction of EDUCATION & Gender
###################################################################################################


#  logit model 1
logit_model1<-glm(obese~ AGE+ Education+Income+Race+Gender+census_region,family = binomial, data=cdc)
summary(logit_model1)

#  logit model 2
logit_model2<-glm(obese~ agegroup+ Education+Income+Race+Gender+census_region,family = binomial, data=cdc)
summary(logit_model2)


#  logit model 3
logit_model3<-glm(obese~ agegroup+ Education+Income+Race+Gender+census_region+ Education*Gender,family = binomial, data=cdc)
summary(logit_model3)



#MODEL COMPARISONS 
##################################################################################################

# Which is a better model? Compare AGE is  continous (model1) or discrete (model2)
# Lower values of AIC & BIC indicate better Model
# Model3 outperforms Model 1 & 2. 

AIC (logit_model1 , logit_model2, logit_model3)




#########################################################################################
# Predicted probabilities. 
# Get predicted probabilies and save the file to study in Tableau
#########################################################################################

#Logit Model 1
cdc$plogit1<- predict(logit_model1, type="response")


#Logit Model 2
cdc$plogit2<- predict(logit_model2, type="response")


# Logit Model 3
cdc$plogit3<- predict(logit_model3, type="response")

#####################################################################################
#Save your Data And Analyze in Tableau
#####################################################################################

write.csv(cdc, file="cdc_prediction.csv" )

