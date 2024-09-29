### Hypothesis testing and the binomial distribution


## Working with observed binomial data in R

# The hand calculation for this is a bit of a chore, but the good news is that 
# there is an easy way to calculate the probability of a single event.

# Our class has 34 women out of 39 students, so Pr[ladies] = 0.872. 
# If I chose students at random from our class in groups of 5. Calculate the 
# probability of randomly selecting 3 women out of those 5 when the probability 
# of a hit (selecting a woman) is 87.2%

dbinom(3, size = 5, prob = 0.872)

# What is the probability of getting 3 or more hits in groups of 5?
# This is Pr[3 or more] = Pr[3] + Pr[4] + Pr[5]

dbinom(4, size = 5, prob = 0.872)
dbinom(5, size = 5, prob = 0.872)
# then sum those probabilities

# Or you could use this: calculate the number of hits from 3 to 5
xsuccesses <- 3:5

# do each calculation
probx <- dbinom(xsuccesses, size = 5, prob = 0.872)

# make a table from those two values
probTable <- data.frame(xsuccesses, probx) 

# display the table
probTable 

# Another way to make the histogram: a barplot with 0 space between the bars
barplot(height = probx, names.arg = xsuccesses, space = 0, las = 1, col = "deepskyblue",
        ylab = "Probability", xlab = "Women in biostats")


## Hypothesis testing with the binomial distribution (general format)

# calculate the number of hits from 0 to 10
xsuccesses <- 0:10

# do each calculation, assume that Pr[X] = 0.28
probx <- dbinom(xsuccesses, size = 50, prob = 0.28)

# make a table from those two values
probTable <- data.frame(xsuccesses, probx) 

# display the table
probTable 

# Use this function to sum up those probabilities
sum(probx)  

# OR to do the one-tailed test in one step, use this:
binom.test(10, n = 50, p = 0.28, alternative = "less")


## Example from a spreadsheet

# bring in the survey data
drinks <- read.csv(url("https://www.dropbox.com/s/16qr93rm15ma3si/drinks.csv?dl=1"))

# count the number of coffee drinkers
nullTable <- table(drinks, dnn = "coffee")/length(drinks)

# make this value in the data frame format (like a spreadsheet)
data.frame(nullTable)

# calculate the probability of coffee drinkers
prCoffee <-  (sum(drinks$response == "coffee"))/(length(drinks$response))

# calculate the single probability of getting 2 coffee drinkers out of 4
dbinom(2, size = 4, prob = prCoffee)

# Test to see if having 8 coffee drinkers out of 10 is significantly different than what 
# would be expected

binom.test(8, n = 10, p = prCoffee, alternative = "greater")


## Sampling distribution (CI) of the proportion

# There is an add-on package called "binom" that will bring in more functions 
# than what is in base R. The following line only needs to be added once:

install.packages("binom", dependencies = TRUE)  ## Run this just once to install

# Load the binom package each session that you want to use it
library(binom) 

# calculate the 95% CI
binom.confint(29, 50, method = "ac")

# Or see all the 95% CIs:
binom.confint(10, 50)



## Hypothesis testing with resampled binomial data
 
# Obtain the null distribution for the test statistic (the number of wins out of 20 games).
# Take a very large number of samples of 20 games from a theoretical population in which 
# the Red Sox win 56.0% of the time (as stipulated by the null hypothesis). For each 
# sample, the number of right-handed toads is calculated and saved in a vector named 
# results20 (the samples themselves are not saved). The results vector is initialized 
# before the loop. The term results20[i] refers to the ith element of results20, 
# where i is a counter. 

results20 <- vector() # makes a blank vector object

# This samples 10,000 runs 20 hypothetical games of the Boston Red Sox, with the outcome
# based on their winning percentage as of 9/24/2020.

for(i in 1:10000){
	tempSample <- sample(c("W", "L"), size = 20, prob = c(0.560, 0.440), replace = TRUE)
	results20[i] <- sum(tempSample == "W")
	}

# Save results20 also as a factor with 21 levels, representing each possible integer 
# outcome between 0 and 20. This step is for tables and graphs only -- it makes sure 
# that all 21 categories are included in tables and graphs, even categories with no 
# occurrences.

factor20 <- factor(results20, levels = 0:20)

# Tabulate the relative frequency distribution of outcomes in the random sampling 
# process. This is the null distribution. The table command calculates 
# the frequencies, and dividing by the length of results18 (i.e., the number of elements 
# in the vector, equal to the number of iterations in the loop) yields the proportions 
# (relative frequencies). 

nullTable <- table(factor20, dnn = "wins")/length(factor20)
data.frame(nullTable)

# Draw a histogram of the null distribution: the number of wins out of 20. 
# Setting freq = FALSE causes hist to graph density of observations rather than 
# frequency. In this case, height of bars indicates relative frequency if bars are 
# one unit wide (as in the present case).  

hist(results20, right = FALSE, freq = FALSE, main = "", col = "red",
	xlab = "Number of wins", xlim = c(0,20), ylim = c(0, 0.2))

# Highlight lower tail
hist(results20, right = FALSE, freq = FALSE, main = "", col = c("red", "red", "red", "red", "red", 
    "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white",
    "white", "white", "white", "white", "white"),
     xlab = "Number of wins", xlim = c(0,20), ylim = c(0, 0.2))

# Highlight both tails
hist(results20, right = FALSE, freq = FALSE, main = "", col = c("red", "red", "red", "red", "red", 
    "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white",
    "red", "red", "red", "red", "red"),
     xlab = "Number of wins", xlim = c(0,20), ylim = c(0, 0.2))

# Calculate the fraction of samples in the null distribution having 4 or fewer wins

frac4orFewer <- sum(results20 <= 4)/length(results20)

frac4orFewer

# Calculate the approximate P-value. 

2 * frac4orFewer



