### Describing samples

## Basic sample statistics

## Sample mean (or arithmetic mean)

a <- sample(1:100, 10, replace = TRUE) 
# This creates a vector that is a sample of 10 whole numbers 
# taken randomly from 1 to 100 with replacement

runif(100, min = 1, max = 100) # samples from a uniform distribution with decimals.

data <- sort(a, decreasing = FALSE)  # ranks the observations in the sample

mean(data)  # the sample mean for an array


data("Orange")  # bring in the built-in dataset about the growth of orange trees

mean(Orange$age)  # the sample mean for one column of a data frame


# getting means of separate groups

data("chickwts")  # this too, about weights of chickens raised on different types of feed

View(chickwts)  # shows the first few rows of data

mean(chickwts$weight)  # calculates the mean of ALL the data

tapply(chickwts$weight, chickwts$feed, mean)  # calculate the means of each group in the sample

# The general form of tapply() is X = a vector, INDEX = list of one or more factor, 
# and FUN = Function or operation that needs to be applied. Note that changing the function 
# part will allow you to calculate other sample statistics across groups in 
# a data frame: sd, median, length (i.e., sample size), quantile, etc.

## Standard Deviation

sd(data)

sd(Orange$age)

tapply(chickwts$weight, chickwts$feed, sd)  # calculate the SD of each group in the sample



## The Median

median(data)  # gives the 50% line of the sample

boxplot(Orange$age, col = "goldenrod", ylim = c(0,1700), ylab = "Age of tree (da)") 

quantile(Orange$age)  # gives all the quartiles of a sample, if that's what you want


### Try the Sampling from a Normal Distribution simulation, located
### at https://www.zoology.ubc.ca/~whitlock/Kingfisher/SamplingNormal.htm


## The Standard Error

sd(Orange$age) / sqrt(length(Orange$age))  
# sqrt() is the square root function
# length() counts the data for the sample size

OrangeAgeSE <- sd(Orange$age) / sqrt(length(Orange$age)) 
# a measure of the spread of the sampling distribution of the sample mean 




### More complicated manipulations

## Summary statistics and subdividing larger datasets

# Bring in Massachusetts COVID-19 data from January 29 to July 4, 2020
covid19 <- read.csv(url("https://www.dropbox.com/s/44k9otvqdtkjhpv/CasesByDate.csv?dl=1"))

View(covid19) # Put the object in the script window for inspection


## What if we wanted to know the average new cases each day in April?

# Tell R to read the text strings in the Date column to a date format and convert
covid19$Date <- as.Date(covid19$Date, format = "%m/%d/%y")
# as.Date() turns dates to yyyy-mm-dd format

# Calculate the mean of new positive cases in April only, based on variable's date values
## Date values are in brackets
mean(covid19$Positive.New[covid19$Date >= "2020-04-01" & covid19$Date <= "2020-04-30"])


# Can also make a new object comprising April cases only, if you want that
aprilCases <- covid19[covid19$Date >= "2020-04-01" & covid19$Date <= "2020-04-30",]

mean(aprilCases$Positive.New, na.rm = TRUE) 
#na.rm = TRUE is a logical argument that omits blank values 

plot(aprilCases$Positive.New, type = "l", ylab = "Number of new cases", 
     xlab = "April 2020", xlim = c(2,30)) # make a line plot


# Calculate proportion of probable cases

covid19$Probable.Prop <- covid19$Probable.New/(covid19$Positive.New + covid19$Probable.New)

plot(covid19$Probable.Prop, type = "l", 
     ylab = "Proportion of probable cases") # for a line plot

# or try to plot by omitting the days with NaN entries
covid19Clean <- na.omit(covid19)

plot(covid19Clean$Probable.Prop, type = "l", 
     ylab = "Proportion of probable cases") # for a line plot)


