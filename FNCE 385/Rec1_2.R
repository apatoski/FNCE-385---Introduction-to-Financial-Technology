#Recitation 1
rm(list = ls())
                     #Empirical Exercise
#Load Data
wage04 <- read.csv("output04_update.csv",header=TRUE)
dim(wage04)
head(wage04)

#Wage and Age Analysis
wage <- wage04$WAGE
age <- wage04$AGE

#Percentile 95%
quantile(wage,probs=0.95)

#Histogram
hist(wage)

#Scatterplot
plot(age, wage,main="Scatterplot of Wage over Age", col = 'blue')
abline(lm(wage~age), col = 'red')

#Regression Model
summary(lm(wage~age))

#Setting Up EDUC
educ <- wage04$EDUC

#Hypothesis Test on population's average years of education is 12
n = length(educ)
tstat <- sqrt(n)*(mean(educ)-12)/sd(educ)
#5% significance level test
reject <- (abs(tstat)>qnorm(0.975)) # critical value for two-sided test using normal distribution
print(reject)

#95% Confidence Interval for population's average years of education
CI_l <- mean(educ)-sd(educ)/sqrt(n)*qnorm(0.975)
CI_u <- mean(educ)+sd(educ)/sqrt(n)*qnorm(0.975)
print(c(CI_l,CI_u))

                     #Monte Carlo Simulation
#Simulate t-statistic's sampling distribution, with f=Bernoulli and p=0.3
#With 1000 Replications and each sample having 500 observations
tstat <- matrix(0,10000)
for (i in 1:10000){
  data <- rbinom(500,1,0.3)
  tstat[i] <- sqrt(length(data))*(mean(data)-0.3)/sd(data)
}
#Does it look Normal?
#Histogram
hist(tstat,50)

install.packages("psych")
library("psych")
#Summary Statistic
describe(tstat)
