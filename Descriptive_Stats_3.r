#pnorm(z) returns the area under the curve from -inf to 
#z (cumulative probability) of the pdf of the normal distribution where z is a Z-score
#i.e. does the job of Table lookup
pnorm(0)
pnorm(0.12) #example in our slides

#examination of 68-95-99.7 rule
pnorm(1)-pnorm(-1)  #Recall that 68% of data  within 1 standard dev. of mean
pnorm(2)-pnorm(-2)  #Recall that 95% of data  within 2 standard dev. of mean
pnorm(3)-pnorm(-3)  #Recall that 99.7% of data  within 2 standard dev. of mean

#pnorm() function allows you to be lazy and to provide mean and sd as well instead of z-score
#example x=6, mu=3, sd=2 so z=(6-3)/2=1.5 so you can use pnorm(1.5) OR pnorm(6,mean=3,sd=2)
pnorm(1.5)
pnorm(6,mean=3,sd=2)
pnorm(266,mean=235,sd=20) #Example I in our slides
pnorm(800,mean=760,sd=146)-pnorm(710,mean=760,sd=146) #Example II part A
#The small difference in answers is because of the rounding errors 

#pnorm by defult returns the area under the lower tail i.e. P[X = x] but you can change this.
pnorm(1.5,lower.tail = FALSE) #This is the upper 
# the sum is obviously 1
pnorm(1.5,lower.tail = FALSE)+pnorm(1.5,lower.tail = TRUE)
pnorm(730,mean=760,sd=146,lower.tail = FALSE) #Example II part B
#The small difference in answers is because of the rounding errors 

###############################################################
#qnorm is the  is the inverse of pnorm. The idea behind qnorm is that you give it a probability, 
#and it returns the z-score whose cumulative distribution matches the probability. 
#No need for reverse table reading! 

qnorm(0.5)
#Similarly, you can define mean and sd so it returns the observatin value instead of the z-score.
qnorm(0.9,mean=7.55,sd=1.2) #Example III
qnorm(0.93) #Part of Example IV
##############################
#rnorm() generates random numbers that follow normal distribution, defult mu=1,sd=1 
Test<-rnorm(10000,mean=3,sd=2) # 10000 random numbers with normal distribution, mean=3,sd=2
hist(Test)
hist(Test,n=100) #use 100 split bins i.e. higher resolution 