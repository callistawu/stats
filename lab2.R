####Stat 10 - Lab #2
## Name: Callista Wu
## SID: 904886275
## TA: Gabriel Ruiz
## Lab/Discussion Time: 1C/10AM

######################################################
#Lab 2 - Data Cleaning/Preparation and Visualization

######################################################
###Intro to Logical Statements/Relational Operators

#Relational Operations
4>3 #is 4 greater than 3?
c(3,8) >= 3 #is 3 or 8 greater than or equal to 3?
c(3,8) <= 3 #is 3 or 8 less than or equal to 3?
c(1,4,9) == 9 #is 1,4, or 9 exactly equal to 9?
c(1,4,9) !=9 #is 1,4, or 9 not (exactly) equal to 9?

##Create an object with the baby weights from NCbirths

#Applications of logical statements: calculations
sum(NCbirths$weight > 100) #the number of babies that weighed more than 100 ounces
mean(NCbirths$weight > 100) #the proportion of babies that weighed more than 100 ounces
mean(NCbirths$gender == "Female") #the proportion of female babies
mean(NCbirths$gender != "Male") #gives the proportion of babies not assigned male

#Applications of logical statements: subsets
fem_weights <- NCbirths$weight[NCbirths$gender == "Female"]

######################################################
###Exercise 1
#a) Reading in data
head(flint)
class(flint)

#b) Dangerous lead levels
library(mosaic)
dangerousPb_indicator = (flint$Pb >= 15)
tally(~dangerousPb_indicator,format="proportion")
sum(dangerousPb_indicator)/length(dangerousPb_indicator)

#c) Mean Copper Level for only test sites in North region
north_flint = flint[flint$Region=="North",]
#finally: mean copper level
mean(north_flint$Cu)

#d) Mean Copper Level for only test sites in North region
dangerousPb_flint = flint[flint$Pb>=15,]
#finally: mean copper level
mean(dangerousPb_flint$Cu)

#e) Mean Lead and Copper Levels
mean(flint$Pb)
mean(flint$Cu)

#f) Create a box plot for the lead levels
boxplot(x = flint$Pb, xlab="Lead", ylab="Amount of Lead Levels", main="Lead Levels in Flint")

#g) Mean vs. median for lead levels in Flint
#no, the mean is not the best measure of center
#because the distribution of data is not symmetric
mean(flint$Pb)
median(flint$Pb)

######################################################
###Exercise 2

#reading data
life <- read.table("http://www.stat.ucla.edu/~nchristo/statistics12/countries_life.txt",header=TRUE)
head(life)

#a) scatterplot of 'life' variable against 'income' variable
#   both in 'life' data.frame
plot(x=life$Income, y=life$Life,
     xlab="Income", ylab="Life Expectancy", main="Life Expectancy vs. Income")

#b) boxplot and histogram of 'income'
hist(life$Income, xlab="Income", ylab="Life Expectancy", 
     main="Life Expectancy vs. Income")
boxplot(life$Income, xlab="Income", ylab="Life Expectancy", 
        main="Life Expectancy vs. Income")

#c) subset 'life' data.frame into two parts:
# one for which income < 1000, the other for which income > 1000
below1k <- life[life$Income<1000,]
above1k <- life[life$Income>=1000,]

#d) use 'below1k' data.frame. plot 'life' vs. 'income' and compute correlation
plot (x=below1k$Income, y=below1k$LIfe,
      xlab="Income", ylab="Life Expectancy", main="Life Expectancy vs. Income")
cor (x=below1k$Income, y=below1k$Life)

######################################################
###Exercise 3

#reading data
maas <- read.table("http://www.stat.ucla.edu/~nchristo/statistics12/soil.txt", header=TRUE)

#a) summary statistics for lead and zinc
summary(maas$lead)
summary(maas$zinc)

#b) histogram of lead and log(lead)
hist(maas$lead)
hist(log(maas$lead))

#c) log(lead) vs. log(zinc)
plot(x=log(maas$zinc),y=log(maas$lead),
     xlab="log(zinc)", ylab="log(lead)")

#d) color scheme for lead concentration risk for 155 locations
lead_colors <- c("green", "orange", "red")
lead_levels <- cut(maas$lead, c(0,150,400,1000))

#plot
plot(maas$x, maas$y, cex=maas$lead/mean(maas$lead),
     col=lead_colors[as.numeric(lead_levels)], pch=19)

######################################################
###Exercise 4

LA <- read.table("http://www.stat.ucla.edu/~nchristo/statistics12/la_data.txt", header=TRUE)

#a) plot the data point locations and overlay a map
library(maps)
plot(x=LA$Longitude, y=LA$Latitude, ylab="label", xlab="label",
     main="title", xlim=c(-120,-117), ylim=c(33,35))
map("county", "california", add = TRUE)

#b) relationship between income and school performance?
# ignore data points on the plot for which schools = 0
LA.subset <- LA[LA$Schools>0,]
plot(x=LA.subset$Income, y=LA.subset$Schools)
