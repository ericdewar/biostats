#### Chi-square tests

### Chi-square goodness-of-fit test (example 1)

# make a vector of observed *values*
obs <- c(84, 16)  

# make a vector of expected *proportions*. 
#   NOTE: A fractional format will always work, but R will choke if the proportions 
#   don't add up to 1.0 exactly

exp <- c(3/4, 1/4)

# do the test
chisq.test(x = obs, p = exp)

# this would be the critical value for rejection if you needed to look it up
qchisq(.95, df=1)        # if 1 degree of freedom


## Chi-square goodness-of-fit test (example 2)

# Bring in the data
# Download the "nhl.csv" file from Canvas datasets 

file.choose() # locate the downloaded file

nhl <- read.csv() # observed values only

# Generate expected probabilities in a new column
nhl$prop <- c(31/365, 28/365, 31/365, 30/365, 31/365, 30/365, 
              31/365, 31/365, 30/365, 31/365, 30/365, 31/365)

# Generate expected values in a new column (if desired)
nhl$exp <- c(31/365, 28/365, 31/365, 30/365, 31/365, 30/365, 
             31/365, 31/365, 30/365, 31/365, 30/365, 31/365) * 1245

nhl #see the table

# Do the test
chisq.test(x = nhl$obs, p = nhl$prop)


## Making a summary table from raw data

# Put this linked file from WS3e into memory  
# bring in the data making the words as "factors"
sd2 <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter08/chap08q18snapdragonGeneticsB.csv"), stringsAsFactors = TRUE)

# Make the table
sd2Table <- table(sd2$color)

# Inpect the table for the observed values
sd2Table




### Chi-square goodness-of-association test

# Bring in the data
# Download the "lesions" file from Canvas datasets 

file.choose() # locate the downloaded file
lesions <- read.csv() # insert path to file 

# make grouping variables using the age column
lesions$age <- factor(lesions$age, 
                      levels = c("30-39", "40-49", "50-59", "60-69"))

# make those data a table to turn the observations into counts
lesionsTable <- table(lesions$age, lesions$severity)

lesionsTable # report back what the table looks like

# and add the marginal totals on the table
addmargins(lesionsTable)

# If you want a mosaic plot of these data
mosaicplot( t(lesionsTable), col = c("red2", "yellow", "green3", "blue"), 
            cex.axis = 1, main = "", las = 1,
            sub = "Lesion severity", ylab = "Age")

# If you want a grouped bar chart
barplot(lesionsTable, col = c("red2", "yellow", "green3", "blue"), 
            cex.axis = 1, main = "", las = 1,
            sub = "Lesion severity", ylab = "Age")

# If you want a stacked bar chart
barplot(lesionsTable, col = c("red2", "yellow", "green3", "blue"), 
        cex.axis = 1, main = "", las = 1, beside = TRUE,
        sub = "Lesion severity", ylab = "Age")

# do the test of association
chisq.test(lesions$age, lesions$severity, correct = FALSE)




## Fisher's exact test

# Fisher's exact test of association between style of music playing and 
# purchases of wine in a store  

# Bring in the data
# Download the "wine.csv" file from Canvas datasets 

file.choose() # locate the downloaded file
wine <- read.csv() # insert path to file 

# Contingency table 

wineTable <- table(wine$choice, wine$music)
wineTable

# Expected frequencies under null hypothesis of independence. R complains because 
# of the violation of assumptions. Just in case you hadn't noticed.

saveTest <- chisq.test(wine$choice, wine$music, correct = FALSE) 
saveTest$expected

# Fisher's exact test. The output includes an estimate of the odds ratio, but 
# it uses a different method than is used in our book.

fisher.test(wine$choice, wine$music)




## Calculations for Goodness-of-fit of the Poisson probability model 
# (testing for randomness)

# Bring in the data from text example
mydata <- read.csv(url("https://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q02ShadParasites.csv"))

# Turn the raw data into a frequency table
parasiteTable <- table(mydata$numberOfParasites)
data.frame(Frequency = addmargins(parasiteTable))

# estimate the mean number of parasites from the data (not the table)
meanParasites <- mean(mydata$numberOfParasites)
meanParasites

# then estimate the Poisson null distribution
expectedFrequency <- expectedProportion * 238
expectedFrequency # see the expected values

# now show the expected values in a table to confirm that no category has fewer 
# than 1 expected case

expectedFreqTable <- data.frame(c(0:6), expectedFrequency)

expectedFreqTable

# Arrange data if need to combine categories
# You CAN just recombine columns in Excel, but if you want R to do it, 
# the way is a little more complicated. 
# In this case I recomment to do it the dumb way in Excel.

# if you want to plot the obs & exp values, this will work:
hist(mydata$numberOfParasites, right = FALSE, 
     xlab = "Number of parasites", ylim = c(0, 120), col = "gray75", main = "",
     breaks = seq(0, 6, 1), las = 1)

# For a fancier histogram that includes the expected values

hist(mydata$numberOfParasites, right = FALSE, 
     xlab = "Number of parasites", ylim = c(0, 120), col = "yellow3", main = "",
     breaks = seq(0, 6, 1), las = 1)
axis(1, at = seq(0.5, 4.5, 1), xaxt = "n" ) 
lines(expectedFrequency, lwd = 2, col = "blue") 


# Run the test after resorting categories, and using the probabilities
# calculated in Excel. Remember that the probabilities need to add up to 1.0.
# To get that, in Excel sum all the calculated probabilities except for the 
# smallest one and subtract from 1. Use the difference to fill in the last 
# probability. 

chisq.test(x = c(103, 72, 44, 19, 5), 
           p = c(0.388532618,
                 0.367310248,
                 0.173623542,
                 0.054713303,
                 0.015820289))

var(mydata$numberOfParasites / meanParasites)





