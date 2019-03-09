####Stat 10 - Lab #4
## Name: Callista Wu
## SID: 904886275
## TA: Gabriel Ruiz
## Lab/Discussion Time: 1C/10AM

######################################################
#Lab 4 - Simulation, Sampling, and the Central Limit Theorem

######################################################
###Simple Random Sampling

#Use the sample() function to conduct simple random sampling
#from a population by sampling from the row numbers of a data frame

#Use sample() to randomly select n numbers between 1 and 1992
#represents choosing the babies based on ID numbers
#use default argument replace=FALSE to ensure n unique ID's

#Use selected data numbers as an index for rows of observations

#set seed for reproducibility
set.seed(123) 

#select 5 numbers from 1 to 1992
sample.index <- sample(1992, size=5)
sample.index #display the indices we sampled

## [1] 573 1570 814 1757 1870

#Extract the rows in NCbirths that correspond to sample.index
NCbirths[sample.index,]


######################################################
###Exercise 1

pawnee <- read.csv("pawnee.csv", header=TRUE)

#a) 'head' function to see first few rows, and
# 'dim' function to see data.frame dimensions
head(pawnee)
dim(pawnee)

#b) Simple random sample of size 30 from pawnee data.frame
set.seed(1337) #ensures everyone gets same 'random' results
rowsToSample <- sample(x=1:nrow(pawnee), size=30, replace = F)
rowsToSample
pawnee_sample <- pawnee[rowsToSample,]

head(pawnee_sample)

#c) Mean arsenic level from sample took in b, and
#proportion of households experiencing a major health issue
mean(pawnee_sample$Arsenic)
p.hat <- mean(pawnee_sample$New_hlth_issue=="Y")
print(p.hat)

#d) What symbol from lecture would we use for the mean arsenic level in the sample?
#What symbol would we use for the proportion of health issues in the sample?

#e) 90%, 95%, and 99% CI's for true population proportion of
#households experiencing a major health issue
se <- sqrt(p.hat*(1-p.hat)/30) #standard error

##critical values: why do we get the below percentiles?
z1 <- qnorm(p=0.95) #95th percentile on std. normal distribution
z2 <- qnorm(p=0.975) #97.5th percentile on std. normal distribution
z3 <- qnorm(p=0.995) #99.5th percentile on std. normal distribution

#CI's
p.hat+c(-1,1)*z1*se #90% CI
p.hat+c(-1,1)*z2*se #95% CI
p.hat+c(-1,1)*z3*se #99% CI

#f) What would be the bounds of a 100% confidence interval
#for the population proportion?

#g) Checking the actual population proportion of households
#experiencing a major health issue
mean(pawnee$New_hlth_issue=="Y")

#h) Visualizing distribution of arsenic levels for the houses in Pawnee
hist(pawnee$Arsenic, breaks=42, xaxt='n', prob=T,
     xlab="Arsenic Levels", main="Arsenic Levels for Houses in Pawnee")
axis(side=1, at=seq(0,210,l=43), labels=seq(0,210,l=43))

boxplot(pawnee$Arsenic,ylab="Arsenic Level",
        main="Arsenic Levels for Houses in Pawnee")

######################################################
###Exercise 2

#a)Distribution of p hat over many samples:
#Does it really look like a normal distibution as the CLT claims?

#Create objects for common quantities
n <- 30 #sample size
N <- 541 #population size
M <- 1000 #number of samples/repetitions

#Create vectors to store simulated proportions from each repetition
phats <- c() #for sample proportions

#Set seed for reproducibility
set.seed(123)

#Always set the seed outside the loop
#Start the loop. Let i cycle over the numbers 1 to 1000
#(i.e. iterate 1000 times)
for (i in 1:M){
  #take the i-th iteration for the loop represents a single repetition
  #take a simple random sample of size n from the population of size N
  index <- sample(N, size=n)
  #save the random sample in the sample_i data.frame.
  sample_i <- pawnee[index,]
  #compute the proportion of the i-th sample of households with a new health issue
  phats[i] <- mean(sample_i$New_hlth_issue == "Y")
}

#distribution:
hist(phats, prob=T)
curve(dnorm(x,mean(phats), sd(phats)), add=TRUE)

hist(phats,prob=T, breaks = seq(0, max(phats), 1/n))
curve(dnorm(x,mean(phats),sd(phats)), add=TRUE)

#b) mean and standard deviation of phats
mean(phats)
sd(phats)

#c) Do you think the simulated distribution of 
#sample proportions is approximately normal?
#Central Limit Theorem conditions
#look at visual evidence for or against

#d) Using CLT theoretical results, what would you predict the
#mean and standard deviation of the sampling distribution of
#sample proportions to be?
(p_true=mean(pawnee$New_hlth_issue=="Y"))
(se_true=sqrt(p_true*(1-p_true)/n))

p_true - mean(phats) 
#difference between true population proportion and mean of histogram

se_true - sd(phats)
#difference between the true standard error and standard deviation of phat spector

######################################################
###Exercise 3

#a) sampling distribution for x-bar for arsenic levels
#create objects for common quantities
n <- 30 #sample size
N <- 541 #population size
M <- 1000 #number of samples/repetitions

#create vectors to store simulated proportions from each repetition
xbars <- c() #for sample means

#set seed for reproducibility
set.seed(123)

#always set seed outside the for loop
#start the loop
#let i cycle over the numbers 1 to 1000 (i.e. iterate 1000 times)
for (i in 1:M){
  #take i-th iteration of the for loop represents a single repetition
  #take a simple random sample of size n from the population of size N
  index <- sample(N, size=n)
  #save the random sample in the sample_i data.frame.
  sample_i <- pawnee[index,]
  #compute the mean arsenic level of the i-th sample of households
  xbars[i] <- mean(sample_i$Arsenic)
}

#b) sampling distribution of x-bar:
hist(xbars, prob=T)
curve(dnorm(x,mean(xbars), sd(xbars)), add=TRUE)

#c) Do you think the simulated distribution of 
#sample arsenic means is approximately normal?
#Central Limit Theorem conditions
#look at visual evidence for or against
