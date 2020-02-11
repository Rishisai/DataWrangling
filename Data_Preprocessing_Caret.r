# Created:    Aug 26, 2017
# @author     Rouzbeh Razavi, PhD (rrazavi@kent.edu)
# @version:   1.0
# File:       Data_Preprocessing_Caret.r
# Comment:    Data Preprocessing Using Caret 
################################################################
rm(list = ls())
library(caret)
library(corrplot)
#install.packages('RANN')
library(RANN)
########################### Imputation #############################
colMeans(is.na(airquality)) #determine the percentage of NA values per variable

# Median imputation of missing values
preProc_1<-preProcess(airquality, method = c("medianImpute"))
airquality_imputed<-predict(preProc_1, airquality)
colMeans(is.na(airquality_imputed))
summary(airquality_imputed)

# k-NN imputation of missing values
# When using knnImpute data is scaled and centered by default
preProc_2<-preProcess(airquality, method = c("knnImpute"),k=5)
airquality_imputed<-predict(preProc_2, airquality)
colMeans(is.na(airquality_imputed))
summary(airquality_imputed)

# Usinng Bagged Tree imputation of missing values
preProc_3<-preProcess(airquality, method = c("bagImpute"))
airquality_imputed<-predict(preProc_3, airquality)
colMeans(is.na(airquality_imputed))
summary(airquality_imputed)

########################## Find Highly Correlated Variables #########################
cor(airquality,use="complete.obs")
corrplot(cor(airquality,use="complete.obs"))
highlyCorDescr <- findCorrelation(cor(airquality,use="complete.obs"), cutoff = .6)
highlyCorDescr
########################## Transformation #########################

#Scale and Center 
preProc_4<-preProcess(airquality, method = c("scale","center"))
airquality_scaled<-predict(preProc_4, airquality)
summary(airquality_scaled) #notice the mean for all values are 0

preProc_5<-preProcess(airquality, method = c("range"))
airquality_scaled<-predict(preProc_5, airquality)
summary(airquality_scaled) #notice the min and max values 

######################### Dummy Variables ###########################
library(ISLR)
summary(Wage)
simpleModel<-dummyVars(~ race, data = Wage, levelsOnly = TRUE)
race_convereted<-predict(simpleModel, Wage)
head(race_convereted)
######################## Find varibles that are linear combinations of each other ############
airquality_new <-cbind(airquality_imputed, New_Variable=6.5*airquality_imputed$Ozone+2*airquality_imputed$Wind)
head(airquality_new) # see the new variable
findLinearCombos(airquality_new)

library(caret)
mtcars_2<-cbind(mtcars,new_variable_1=1,new_variable_2=c('Reliable'))
summary(mtcars_2)
nzv <- nearZeroVar(mtcars_2)
nzv
mtcars_filtered <- mtcars_2[, -nzv]
summary(mtcars_filtered)

#########################  Near Zero Variance Variables    #############
library(caret)
mtcars_2<- cbind(mtcars, new_variable_1=1,new_variable_2=c('Reliable'))
summary(mtcars_2)
nzv <- nearZeroVar(mtcars_2)
nzv
mtcars_filtered <- mtcars_2[, -nzv]
summary(mtcars_filtered)

####################### PCA ################################
library(caret)
library(corrplot)
#install.packages('AppliedPredictiveModeling')
library(AppliedPredictiveModeling)
data(segmentationOriginal)
segData <- subset(segmentationOriginal, Case == "Train")
# Now remove the columns
segData <- segData[, -(1:3)]
nzv <- nearZeroVar(segData)
segData <- segData[, -nzv]

ncol(segData) # segData has 113 variables (columns) 
tranformation <- preProcess(segData, method = c( "center", "scale", "pca"))
tranformation
segData_transformed <- predict(tranformation, segData)
ncol(segData_transformed) #transformed data has 57 variables (columns) 
