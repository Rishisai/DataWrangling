
# Created:    Jul 10, 2017
# @author     Rouzbeh Razavi, PhD (rrazavi@kent.edu)
# @version:   1.0
# File:       Descriptive_Stats_1.r
# Comment:    SDescriptive Stats Part 1
################################################################

#read CSV file, by default the top column is the name of attributes
# hashtags are for comments! 
MyData=read.csv('F:\\Kent Teaching\\Datasets\\Occupancy_Detection.csv'); 
head(MyData)                  #show the top fewlines 
head(MyData$Temperature)      # MyData$Temperature is the Temperature attribute 
mean(MyData$Temperature)      #get the mean of MyData$Temperature
mean(MyData[,1])              #get the mean of the first column of MyData (which is the same as MyData$Temperature)
sd(MyData$Temperature)        #get the standard deviation of MyData$Temperature
var(MyData$Temperature)       #get the variance of MyData$Temperature
quantile(MyData$Temperature)  #get the quantile of MyData$Temperature
#install.packages('moments')   #install the 'moments' package which has an implemention of Skewness statistics
library(moments)
skewness(MyData$Temperature)
range(MyData$Temperature)
summary(MyData$Temperature)   #5-number summary (min, Q1, Median, Q3, max) plus mean of MyData$Temperature
sapply(MyData,mean)
sapply(MyData,var)
sapply(MyData,quantile)
boxplot(MyData[,c(1,2)])

#%%%%%%%%%%%%%%%%%%%%%%%%%% Effect of Missing Values %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

MyData=read.csv('F:\\Kent Teaching\\Datasets\\Occupancy_Detection_Missing.csv'); 
mean(MyData$Temperature) 
var(MyData$Temperature)
mean(MyData$Temperature,na.rm=TRUE) #This removes missing values before calcuatig stat
var(MyData$Temperature,,na.rm=TRUE)

