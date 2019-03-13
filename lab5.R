####Stat 10 - Lab #5
## Name: Callista Wu
## SID: 904886275
## TA: Gabriel Ruiz
## Lab/Discussion Time: 1C/10AM

######################################################
#Lab 5 - Hypothesis Testing Bonanza!

######################################################
# Exercise 1: Hypothesis testing with one proportion

#Set working directory
flint <- read.csv("flint_2015.csv", header=TRUE)

#a) null vs alternative hypotheses
# one-sided vs two-sided tests

#b) Sample proportion and sample standard deviation for households with dangerous lead levels.
n <- nrow(flint)
dangerous_lead_indicator <- (flint$Pb >= 15)
p_hat <- mean(dangerous_lead_indicator)
sd_sample <- sqrt(p_hat*(1-p_hat)/n)

print(p_hat)
print(sd_sample)

#c) Standard Error and z-statistic under the Null: p=0.10.
p_null <- 0.10
se_null <- sqrt(p_null*(1-p_null)/n)
z_stat <- (p_hat-p_null)/se_null
print(z_stat)
print(se_null)

#d) p-value for above z-statistic and alternative hypothesis:
# H1: p > 0.10.
# Note: pnorm function gives probability to the left of input value
# in standard normal distribution.
p_value <- 1-pnorm(z_stat,sd=1,mean=0)
print(p_value)

# Review question 1: If we actually had the alternative hypothesis
# "H1: p < 0.10", would we take the probability to the right or left
# of our z_stat?

# Review question 2: If we actually had the alternative hypothesis
# "H1: p is not equal 0.10", how would we calculate the p value?

#e) Reject null hypothesis?

#f) See lab manual.

#g) Same hypothesis test with ’prop.test’ function in Mosaic.
library(mosaic)
prop.test(x=sum(dangerous_lead_indicator),n=n,p=0.10,alternative="greater")

#h) 99% Confidence interval with "greater than" alternative hypothesis.
prop.test(x=sum(dangerous_lead_indicator),n=n,p=0.10,
          alternative="greater",conf.level = 0.99)

######################################################
# Exercise 2: Hypothesis testing with two proportions

flint <- read.csv("flint_2015.csv", header=TRUE)

#a) null and alternative hypotheses
# one-sided or two-sided test?

#b) Computing necessary values for z statistic.
flint_north <- flint[flint$Region=="North" , ]
n_north <- nrow(flint_north) #sample size for north
flint_south <- flint[flint$Region=="South" , ]
n_south <- nrow(flint_south) #sample size for south
#Sample proporition of households with dangerous Pb levels.
p_hat_north <- mean(flint_north$Pb>=15)
p_hat_south <- mean(flint_south$Pb>=15)
#Pooled Proportion of households with dangerous Pb levels.
#Note: same p hat as earlier.
p_hat_pooled <- mean(flint$Pb >= 15)
#Standard deviation for two proportion test (see lab manual).
SE <- sqrt( p_hat_pooled*(1-p_hat_pooled) * (1/n_north + 1/n_south) )
#Finally, the z-statistic for a 2 proportion test:
z_stat <- (p_hat_north-p_hat_south-0)/SE
print(z_stat)

#c) P-value for this test.
# Why do we multiply by 2?
p_value <- (1-pnorm(z_stat,sd=1,mean=0) )*2
print(p_value)

#d) Reject null hypothesis?

#e) 2 sample proportion hypothesis test using 'prop.test' in mosaic package.
library(mosaic)
x_north <- sum(flint_north$Pb>=15)
x_south <- sum(flint_south$Pb>=15)
prop.test(x=c(x_north,x_south),n=c(n_north,n_south),
          alternative="two.sided")


######################################################
# Exercise 3: Hypothesis testing with means

flint <- read.csv("flint_2015.csv", header=TRUE)

#a) null and alternative hypotheses
#one-sided or two-sided test?

#b) Mean and SD of Cu levels in Flint, MI.
( xbar <- mean(flint$Cu) )
( s <- sd(flint$Cu) )

#c) Standard error of xbar. Refer to lecture notes.
n <- nrow(flint)
(SE <- s/sqrt(n))

#d) t statistic and P value for this test using t distribution with n-1 df.
(t_stat <- (xbar-40)/SE)
p_value <- (1-pt(t_stat,df=n-1) )*2
print(p_value)

#e) Reject null hypothesis?

#f) Same test with 't.test' function in mosaic package.
library(mosaic)
t.test(flint$Cu, mu = 40, alt = "two.sided", conf.level = 0.99)


######################################################
# Extra Credit: Hypothesis testing in regression

soil <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c173_c273/soil_complete.txt",
                   header=TRUE)

linear_model <- lm(soil$lead ~ soil$zinc)
summary(linear_model)
