# Set your Work directory & read data

setwd ("C:/Users/vsingh.NYC-STERN/Dropbox/teaching/2014/Fall/Assignments/Final/")
soda<- read.csv("Coke_pepsi_competition.csv", header = TRUE)


# Create log of sales & Price
# Mutate is a commnad to create new variables

library(dplyr)

soda1 <- mutate(soda, log_coke_unit= log(Coke_unit), log_pepsi_unit=log(Pepsi_unit), 
                log_coke_price=log(Coke_price), log_pepsi_price=log(Pepsi_price))
                
               
coke_reg <-lm (log_coke_unit~ log_coke_price+log_pepsi_price+ Market, data=soda1)
summary(coke_reg)

pepsi_reg <-lm (log_pepsi_unit~ log_coke_price+log_pepsi_price+ Market, data=soda1)
summary(pepsi_reg)

library(texreg)
screenreg(list(coke_reg, pepsi_reg) , single.row=TRUE, custom.model.names= c("COKE" , "PEPSI"))
