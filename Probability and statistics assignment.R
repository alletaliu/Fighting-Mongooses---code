#Exercise 1
BMI <- rnorm(100, 25, 2.5)
#Since when BMI=20, cholesterol=140 and when BMI=30, cholesterol=220, the linear function between cholesterol and BMI is y=8x-20 (y is cholesterol and x is BMI). Thus, the cholesterol is coded using 8*BMI-20 as the fixed component with error that has a standard deviation of 20. 
CH1 <- 8*BMI-20+rnorm(100,0,20)
plot(x = BMI, y = CH1, xlab='BMI', ylab='cholesterol (mg/dL)', main = 'cholesterol against BMI')
#There is a uptrend linear correlation between chelesterol and BMI, although it does not seem to be very strong. 
CH2 <- 8*BMI-20+rnorm(100,0,100)
plot(x = BMI, y = CH2, xlab='BMI', ylab='cholesterol2 (mg/dL)', main = 'cholesterol2 against BMI')
#The dots in this graph seems to be more scattered and the correlation in this case seems to be much weaker. This could because the variance of cholesterol becomes much larger. 

#Exercise 2
#part 1
ex2 <- runif(10000, min = 0, max = 50)
#part 2
hist(ex2)
mean1 <- mean(ex2)
var1 <- var(ex2)
#part 3
n2 <- 2; rows2 <- 10000
sim2 <- runif(n2*rows2, min=0, max=50)
m2 <- matrix(sim2, rows2)
#part 4
sample.means2 <- rowMeans(m2)
#part 5
hist(sample.means2)
mean2 <- mean(sample.means2)
var2 <- var(sample.means2)
#part 6
#for sample size 4
n4 <- 4; rows4 <- 10000
sim4 <- runif(n4*rows4, min = 0, max = 50)
m4 <- matrix(sim4, rows4)
sample.means4 <- rowMeans(m4)
hist(sample.means4)
mean4 <- mean(sample.means4)
var4 <- var(sample.means4)
#for sample size 8
n8 <- 8; rows8 <- 10000
sim8 <- runif(n8*rows8, min = 0, max = 50)
m8 <- matrix(sim8, rows8)
sample.means8 <- rowMeans(m8)
hist(sample.means8)
mean8 <- mean(sample.means8)
var8 <- var(sample.means8)
#for sample size 16
n16 <- 16; rows16 <- 10000
sim16 <- runif(n16*rows16, min = 0, max = 50)
m16 <- matrix(sim16, rows16)
sample.means16 <- rowMeans(m16)
hist(sample.means16)
mean16 <- mean(sample.means16)
var16 <- var(sample.means16)
#for sample size 32
n32 <- 32; rows32 <- 10000
sim32 <- runif(n32*rows32, min = 0, max = 50)
m32 <- matrix(sim32, rows32)
sample.means32 <- rowMeans(m32)
hist(sample.means32)
mean32 <- mean(sample.means32)
var32 <- var(sample.means32)
#for sample size 64
n64 <- 64; rows64 <- 10000
sim64 <- runif(n64*rows64, min = 0, max = 50)
m64 <- matrix(sim64, rows64)
sample.means64 <- rowMeans(m64)
hist(sample.means64)
mean64 <- mean(sample.means64)
var64 <- var(sample.means64)
#plot of mean of each sample mean against size number
Xsample.means <- rbind(mean1, mean2, mean4, mean8, mean16, mean32, mean64)
Nsample.means <- rbind(1, n2, n4, n8, n16, n32, n64)
plot(x = Nsample.means, y = Xsample.means, xlab = 'sample size', ylab = 'mean of sample means', main = 'Mean of sample means against sample size')
#plot of variance of each sample mean against reciprocal of size number
Vsample.means <- rbind(var1, var2, var4, var8, var16, var32, var64)
RNsample.means <- rbind(1/1, 1/n2, 1/n4, 1/n8, 1/n16, 1/n32, 1/n64)
plot(x = RNsample.means, y = Vsample.means, xlab = 'reciprocal of sample size', ylab = 'variance of sample means', main = 'Variance of sample means vs. reciprocal of sample size')
#The above is all for uniform distribution
#for poisson distribution
#for sample size 1
Pex2 <- rpois(10000,0.5)
hist(Pex2)
Pmean1 <- mean(Pex2)
Pvar1 <- var(Pex2)
#for sample size 2
Pn2 <- 2; Prows2 <- 10000
Psim2 <- rpois(Pn2*Prows2, 0.5)
Pm2 <- matrix(Psim2, Prows2)
Psample.means2 <- rowMeans(Pm2)
hist(Psample.means2)
Pmean2 <- mean(Psample.means2)
Pvar2 <- var(Psample.means2)
#for sample size 4
Pn4 <- 4; Prows4 <- 10000
Psim4 <- rpois(Pn4*Prows4, 0.5)
Pm4 <- matrix(Psim4, Prows4)
Psample.means4 <- rowMeans(Pm4)
hist(Psample.means4)
Pmean4 <- mean(Psample.means4)
Pvar4 <- var(Psample.means4)
#for sample size 8
Pn8 <- 8; Prows8 <- 10000
Psim8 <- rpois(Pn8*Prows8, 0.5)
Pm8 <- matrix(Psim8, Prows8)
Psample.means8 <- rowMeans(Pm8)
hist(Psample.means8)
Pmean8 <- mean(Psample.means8)
Pvar8 <- var(Psample.means8)
#for sample size 16
Pn16 <- 16; Prows16 <- 10000
Psim16 <- rpois(Pn16*Prows16, 0.5)
Pm16 <- matrix(Psim16, Prows16)
Psample.means16 <- rowMeans(Pm16)
hist(Psample.means16)
Pmean16 <- mean(Psample.means16)
Pvar16 <- var(Psample.means16)
#for sample size 32
Pn32 <- 32; Prows32 <- 10000
Psim32 <- rpois(Pn32*Prows32, 0.5)
Pm32 <- matrix(Psim32, Prows32)
Psample.means32 <- rowMeans(Pm32)
hist(Psample.means32)
Pmean32 <- mean(Psample.means32)
Pvar32 <- var(Psample.means32)
#for sample size 64
Pn64 <- 64; Prows64 <- 10000
Psim64 <- rpois(Pn64*Prows64, 0.5)
Pm64 <- matrix(Psim64, Prows64)
Psample.means64 <- rowMeans(Pm64)
hist(Psample.means64)
Pmean64 <- mean(Psample.means64)
Pvar64 <- var(Psample.means64)
#plot of mean of each sample mean against size number for poisson distribution
PXsample.means <- rbind(Pmean1, Pmean2, Pmean4, Pmean8, Pmean16, Pmean32, Pmean64)
PNsample.means <- rbind(1, Pn2, Pn4, Pn8, Pn16, Pn32, Pn64)
plot(x = PNsample.means, y = PXsample.means, xlab = 'sample size', ylab = 'mean of sample means', main = 'Mean of sample means against sample size (poisson)')
#plot of variance of each sample mean against reciprocal of size number for poisson distribution
PVsample.means <- rbind(Pvar1, Pvar2, Pvar4, Pvar8, Pvar16, Pvar32, Pvar64)
PRNsample.means <- rbind(1/1, 1/Pn2, 1/Pn4, 1/Pn8, 1/Pn16, 1/Pn32, 1/Pn64)
plot(x = PRNsample.means, y = PVsample.means, xlab = 'reciprocal of sample size', ylab = 'variance of sample means', main = 'Variance of sample means vs. reciprocal of sample size (poisson)')
#Central limit Theorem: the means of samples have a normal distribution regardless of the shape of the original population. 

#Exercise 3
#part 1
fcats <- c(2,1,4,1,2,1,1,2,0,3,0,5)
#Assign the observations of feral cats to fcats
mean(fcats)
#Calculate the mean of observed data, which is 1.83
counter <- 0 #Set up a counter
for(i in 1:1000000){ #Loop through 1 million iterations
  w <- rpois(12,3.5) #Generate random sample that has a size of 12, a mean of 3.5 and a poisson distribution
  if (mean(w)<1.83){counter <- counter+1} #Ask if the mean of the sample generated is less than or equal to 1.83, if it is, the counter will be taken and add 1
  #In this case equal to 1.83 is not part of the script, since this is a continuous random variable, the probability of it will be 0
}
counter <- counter/1000000 #Whenever a mean is less than 1.83 is found, 1 is added to the counter. At the end, the counter will be expressed as a proportion of 1 million
#The above loop returns counter = 0.000268, this is much less than the level of significance of 0.05, so we reject the null hypothesis that the population mean is 3.5
#part 2
fgoats <- c(4,3,6,2,3,2,2,3,2,5,1,7)
#Assign the observations of feral goats to fgoats
mean(fgoats)
#Calculate the mean of observed data from feral goats, which is 3.33
counterg <- 0 #Set up a counterg for feral goats
for(i in 1:1000000){ #loop through 1 million iterations
  w <- rpois(12,3.5) #Generate random sample that has a size of 12, a mean of 3.5 and a poisson distribution
  if (mean(w)<3.33){counterg <- counterg+1} #Ask if the mean of the feral goat sample generated is less than 3.33, if it is, the counterg will be taken and add 1
}
counterg <- counterg/1000000 #Whenever a mean is less than 3.33 is found, 1 is added to the counterg. At the end, the counterg will be expressed as a proportion of 1 million
#The above loop returns counterg = 0.358372, this is much more than the level of significance of 0.05, so we retain the null hypothesis that the population mean of feral goats is 3.5. This also means that the population mean of feral goats do not equal to the population mean of feral cats
