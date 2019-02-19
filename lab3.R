####Stat 10 - Lab #3
## Name: Callista Wu
## SID: 904886275
## TA: Gabriel Ruiz
## Lab/Discussion Time: 1C/10AM

######################################################
#Lab 3 - Linear Regression, Probability, and Sampling

######################################################
###Linear Regression in R

## Run the linear model of weight against Mom's age and print a summary
linear_model <- lm(NCbirths$weight ~ NCbirths$Mage)
summary(linear_model)

## Create a plot of the data, and draw the regression line using abline
plot(NCbirths$weight ~ NCbirths$Mage, xlab = "Mom Age", ylab = "Weight",
     main = "Regression of Weight on Mother's Age")
abline(linear_model, col = "red", lwd = 2)

## Create a plot of the residuals to assess regression assumptions
plot(linear_model$residuals ~ NCbirths$Mage, main = "Residuals plot")

## Add a line of y = 0 to help visualize the residuals
abline(a = 0, b = 0, col = "red", lwd = 2)

######################################################
###Exercise 1: Linear Regression in R

#Reading data
soil <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c173_c273/soil_complete.txt",
                   header=TRUE)

#a)Linear model of lead on zinc
linear_model <- lm(soil$lead ~ soil$zinc)
summary(linear_model)

#b) Plot of lead on zinc with regression line
plot(soil$lead ~ soil$zinc, ylab="Lead Concentration", xlab="Zinc Concentration",
     main="Regression of Lead Concentration on Zinc Concentration")
abline(linear_model, col = "blue", lwd = 2)

#c) Create a blot of the residuals to assess regression assumptions
plot(linear_model$residuals ~ soil$zinc, main = "Residuals Plot",
     xlab="Zinc Concentration", ylab="Residuals")
#Add a line of y=0 to help visualize the residuals
abline(a=0, b=0, col="blue", lwd = 2)


######################################################
###Exercise 2: Linear Regression on Time Series Data

#Set working directory to the folder the data 'sea_ice.csv' is saved in
ice <- read.csv("sea_ice.csv", header = T)

#Reformat 'Date' column so that R knows it corresponds to "date" class
ice$Date <- as.Date(ice$Date, "%m/%d/%Y")

#a) Linear model of sea ice extent against time
linear_model <- lm(ice$Extent~ice$Date)
summary(linear_model)

#b) Plot of sea ice extent over time
plot(ice$Extent~ice$Date, ylab="Sea Ice Extent", xlab="Date",
     main="Regression of Sea Ice Extent on Time")
abline(linear_model, col="blue", lwd=2)

#c) Create a plot of the residuals to assess regression assumptions
plot(linear_model$residuals~ice$Date, main="Residuals plot",
     xlab="Date", ylab="Residuals")
#Add a line of y=0 to help visualize the residuals
abline(a=0, b=0, col="blue", lwd=2)

######################################################
###Exercise 3: Sampling and Simulating in R

#b) Simulating 5,000 craps results
set.seed(123) #setting seed to 123
##Create a vector from 1 to 6 to sample from numbers = 1:6
numbers = 1:6
##Simulate rolling 2 dice, 5000 times
rand_draws = replicate(5000, sample(numbers, 2, replace = TRUE))

#Why is the replace argument set to 'TRUE'?

##Result of the sum of two dice
results = colSums(rand_draws) ##takes the sum of each sample

#visualizing results
barplot(table(results),
        xlab = "Sum of Numbers from Rolling Two Die",
        ylab = "Frequency",
        main = "Frequency of Sum of Numbers from Rolling 2 Die")

#c) Find: (i) Percentage of time Adam doubled his money, and
# (ii) percentage of time Adam lost his money.

#table of proportions for each sum
table(results)/5000

######################################################
###Calculating normal and binomal distribution probabilities

## Coin flipping scenario. Probability of getting 4 heads when 7 coins are tossed
dbinom(4, size = 7, prob = 0.5)

## Probability of getting 4 heads or less when 7 coins are tossed
pbinom(4, size = 7, prob = 0.5)

## Probability of getting a number less than 4 from a normal distribution with mean 2, sd of 7
pnorm(4,mean=2,sd=.7)

######################################################
###Exercise 4: Calculating normal and binomal distribution probabilities

#a)Y:= # of rainy days in 2019
Y~binomial(n=365,p=0.4)

#b) Mean and Standard Deviation of Raid days in 2019
#set the value of n appropriately
n = 365
#set the value of p appropriately
p = 0.4
#calculate the mean
n*p
#calculate the sd
sqrt(n*p*(1-p))

#c) Probability that the park will experience exactly 145 days of heavy rain.
dbinom(145,size=n,prob=p)

#d) Probability that the park will see between 125 and 175 days of heavy rain
pbinom(175,size=n,prob=p)-pbinom(124,size=n,prob=p)
#Why do we subtract 'pbinom(124,size=n,prob=b)' and not something else? Why 124?

#e) Probability that the park will experience more than 230 inches of rain in 2019
1-pnorm(230, mean=200, sd=20)
#Why do we have 1 minus a value?
