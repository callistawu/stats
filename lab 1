####Stat 10 - Lab #1
## Name: Callista Wu
## SID: 904886275
## TA: Gabriel Ruiz
## Lab/Discussion Time: 1C/10AM

######################################################
###Section 1 - R and RStudio Basics

##Setting your working directory

#Set your working directory to the folder where the data you downloaded is saved.
# Session > Set Working Directory > Choose Directory...

######################################################
##Examples of Vectors
numbers <- c(1,2,3,4,5)
schools <- c("UCLA", "UC Berkeley", "USC")

# How would you describe what a "vector" object is?
# a list of numeric or categorical data

# What similarities and differences do you see between
#  the vector that stores numbers, and the vector that
#  stores text?
# numbers just require a list of values,
# names should be in text format with quotes around it

# Try printing out the contents of one of the vectors.
print(numbers)
print(schools)

# Multiply 2 to each element in the vector 'numbers'.
numbers*2

# Access the second element in the 'schools' vector.
numbers[4]
schools[1:2]
schools[c(1,3)]

######################################################
###Section 1 Exercises
##Exercise 1: Vectors.
#a) Vector named 'heights' with your and two neighbors' heights. Print the vector.
heights <- c(61,66,65)
print(heights)

#b) Vector named 'names' with names of you and two neighbors. Print the vector. 
names <- c("Callista", "Janine", "Hannah")
print(names)

#c) Try typing cbind(heights, names). What did this command do? What class is this new object?
cbind(heights, names)
# data frame

##Exercise 2: Downloading data.
#a) Load in 'births.csv' to RStudio.
NCbirths <- read.csv("NCbirths.csv", header=TRUE)
head(NCbirths)

##Important: the file must have a '.csv' extension. 
##If it does not, you did not save the file in the correct format.
##On Macs: ctrl-click link > Download Linked File [first option]
##On Windows: right click link > save linked file as > ...

#If you set the working directory to the folder where 'births.csv' is saved:

#b) Check if things work: 
head(NCbirths)

##Exercise 3: Load the maps package.
#a) Install and verify installation.
install.packages('maps')
find.package("maps")

#if you have not installed yet, highlight and run (excluding the '#' symbols): 
## install.packages('maps')

find.package('maps') #if not installed, you will get an error message. 

#b) Load the 'maps' package.
library("maps") #loads the desired package
map("state")

##Exercise 4: Perform vector operations
# a) Extract the weight variable as a vector from the data frame:
weights=NCbirths$weight

# b) What units do you think the weights are in?
#ounces

# c) Create a new vector named 'weights.in.pounds' which are the weights 
# of the babies in pounds. You can look up conversion factors on the internet.
weights.in.pounds <- (weights*0.0625)

# d) Demonstrate your success by typing weights.in.pounds[1:20] and 
# including the output in your word processing document.
weights.in.pounds <- weights.in.pounds[1:20]

######################################################
### Section 2 – Summarizing Data (one variable)
## Section 2 Exercises.

#If you haven't already installed "mosaic" package, run (with out '#' symbols):
## install.packages('mosaic')

#load the mosaic package: 
library(mosaic)

#Exercise 1: Mean weight of the babies.
tally(NCbirths$Habit)
mean(NCbirths$weight)

#Exercise 2: Percentage of women who smoke in sample.
tally(NCbirths$Habit,format='percent')
# 9.39%.

#Exercise 3: No R work. See lab manual and answer accordingly.

######################################################

### Section 3 – Visualizing Data (one quantitative variable)
## Section 3 Exercises: histogram(), dotPlot(), and boxplot() exercises

#Load package 'mosaic'
library(mosaic)

#If necessary, install 'mosaic' and run previous line again
## install.packages('mosaic')

# Exercise 1: Produce a dot plot of the weights in pounds.
dotPlot(weights.in.pounds, xlab="Weights (pounds)", ylab="Frequency", main="Weights in Pounds")

# Exercise 2: Produce three different histograms of the weights in pounds. 
# Use 3 bins, 20 bins, and 100 bins.
hist(weights.in.pounds)
hist(weights.in.pounds, breaks=seq(from = 0,to = 12, l=4))#3 bins
hist(weights.in.pounds, breaks=seq(from = 0,to = 12, l=21))#3 bins
hist(weights.in.pounds, breaks=seq(from = 0,to = 12, l=101))#3 bins

# Exercise 3: Create a side by side boxplot of the mother’s ages and the father’s ages. 
boxplot(NCbirths$Mage,NCbirths$Fage,
        names=c("Mother","Father"),ylab="Age (years)", main="Mother and Father's Ages")

# Exercise 4: Describe what this code does. And answer the pertinent follow-up question
# in the lab manual. 
histogram(~weight | Habit, data = NCbirths, layout=c(1,2))

######################################################
### Section 4 – Visualizing Data (two categorical)
## Section 4 exercises
# Exercise 1: Find a categorical variable that summarizes a 
# baby's health. Show its relationship to Mother's Smoking Habit.
colnames(NCbirths)#the variables in this data set for you to check.

tally(~Habit | BirthComp, data = NCbirths, format = "proportion")

######################################################
# Section 5 – Visualizing Data (two quantitative)
# Example:
plot(NCbirths$weight ~ NCbirths$Gained)
plot(NCbirths$weight ~ NCbirths$Gained, col = "red", cex = 1.5, pch = 3,
     xlab = "Weight gained during pregnancy", ylab = "Baby weight (oz.)",
     main = "Baby weight vs. pregnancy weight gain")

# Section 5 exercises
# 1. Produce a nicely formatted scatter plot of the weight of 
# the baby vs. the mother’s age. Hint: Change previous examples accordingly.

plot(NCbirths$weight ~ NCbirths$Mage, col = "blue", cex = 1, pch = 1, 
     xlab = "Mother's age (years)", ylab = "Baby weight (oz.)", 
     main = "Baby weight vs. mother's age")

######################################################
### Section 6 – Visualizing Data (geographic data)

## Section 6 exercises
# Exercise 1: Reproduce results in pgs. 8-11 of handout
# with some modifications as referred to in lab manual. 

a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics12/ozone.txt", header=TRUE)
head(a)
install.packages("maps")
library(maps)

#see table on page 9 of handout for what each color designates
#below three lines designate categories (colors) for each
#numeric value of ozone ppm
AQI_colors <- c("blue", "purple", "green", "yellow", "orange")
AQI_levels <- cut(a$o3, c(0, 0.06, 0.075, 0.104, 0.115, 0.374))
as.numeric(AQI_levels) #ensures this vector is numeric

#plot the pretty plot
library(maps)

plot(a$x,a$y, xlim=c(-125,-114),ylim=c(32,43), asp=1, xlab="Longitude",
     ylab="Latitude", main="California ozone bubble plot", "n")
map("county", "ca",add=TRUE)
points(a$x,a$y, cex=a$o3/mean(a$o3),
       col=AQI_colors[as.numeric(AQI_levels)], pch=8)
