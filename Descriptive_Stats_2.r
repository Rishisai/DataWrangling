
# Created:    Jul 10, 2017
# @author     Rouzbeh Razavi, PhD (rrazavi@kent.edu)
# @version:   1.0
# File:       Descriptive_Stats_2.r
# Comment:    SDescriptive Stats Part 2
################################################################

library(ISLR)
plot(Wage$age,Wage$wage)
cor(Wage$age,Wage$wage,method = 'pearson') #Pearson Correlation
cor(Wage$age,Wage$wage)    #defualt method is 'Pearson' so can be omitted
cor(Wage$wage,Wage$age)                #Correlation is symetric
cor((Wage$age*10+1),(Wage$wage*0.2)+7) #Correlation is scale free
###############################################################################

#lets calucate the correlation step by step (i.e. no use of cor function)
numerator=sum((Wage$age-mean(Wage$age))*(Wage$wage-mean(Wage$wage)))
denominator=sqrt(sum((Wage$age-mean(Wage$age))^2))*sqrt(sum((Wage$wage-mean(Wage$wage))^2))
numerator/denominator

###################################################################################
cor(Wage$age,Wage$wage,method = 'spearman') #Spearman Correlation Coefficient 
levels(Wage$education)     #This shows differenet levels of 'education' variable
head(as.numeric(Wage$education)) #This converts the variable to numeric (< HS Grad --> 1 etc)
#head() can be used to show the top 6 records.
cor(as.numeric(Wage$education),Wage$wage,method = 'spearman') #Spearman Correlation Coefficient

###############################################################################
# A simple example to show the difference between Pearson and spearman co

A=c(1,2,3,4,5);
B=c(11,10,14,15,17);
C=c(11,10,14,15,1000);

cor(A,B,method = 'spearman')
#The Spearman Correlation Coefficient dosent change as long as the rank orders are the same
cor(A,C,method = 'spearman')   

#... but Pearson Correlation does!
cor(A,B,method = 'pearson')
cor(A,C,method = 'pearson')

###################################################################################

summary(Carseats) #summary of car seat dataset
Carseat_num<-Carseats[,c(1:6,8,9)] #selecting numerical variables (columns 1-6,8,9)          
pairs(Carseat_num)
install.packages('corrplot')
library(corrplot)
corrplot(cor(Carseat_num), method="color")