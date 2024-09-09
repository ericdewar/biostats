##  Visualizing data in charts and tables. 

### First, use this link to download this spreadsheet in your browser:
##  https://github.com/ericdewar/biostats/blob/master/SKL-bite.csv

## If you're bringing in data from a file that is on your computer's drive, then
# there are a few steps

# 1. You'll need to tell R where to find it. 
file.choose()
# copy that output

# 2. And then define the object using that path:
sklbite <- read.csv() 
## insert the path from the last step inside the parentheses, WITH quotes


## OR you can grab one of R's built-in datasets
data()  # no arguments—gives you a list of available datasets in script pane

data("Nile")  # load the datset about water flow in the Nile River through time

Nile  #  view the object in the console


## OR you can bring in data in CSV format from a website
teeth <- read.csv(url("https://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q06WolfTeeth.csv"))


## Making tables
# Only needed before making a bar chart of outcomes of a categorical variable 

# Make a table of the bite forces
familyTable <- table(sklbite$family) # Makes the table object from the 'family' column

familyTable <- sort(table(sklbite$family), decreasing = TRUE)  # Sorts the table object by *frequency*

familyTable  # reports back the table in the console

data.frame(Frequency = familyTable)  # Make this a vertical table

data.frame(Frequency = addmargins(familyTable))  # and add totals


##  Bar charts

barplot(familyTable) #basic

barplot(familyTable, ylab = "Frequency", las = 2, col = "firebrick") 


##  Histograms

hist(teeth$wolfTeethLength, right = FALSE) #basic

hist(teeth$wolfTeethLength, xlab = "Tooth length (mm)", ylab = "Frequency",
     main ="", col = "firebrick", xlim = c(9,12)) # fancy

hist(teeth$wolfTeethLength, xlab = "Tooth length (mm)", ylab = "Frequency",
     main ="", col = "firebrick", xlim = c(9,12), breaks = seq(9.6, 11.2, by = 0.1)) # makes the bins smaller 


##  Scatterplots

plot(sklbite$bite ~ sklbite$length) #basic
# The argument in the parentheses is Y ~ X

plot(sklbite$bite ~ sklbite$length, las = 1, pch = 16, col = "firebrick", cex = 1.5, 
     bty = "l", xlab = "Skull length (cm)", ylab = "Bite force (N)")


##  Line graphs 
# Used for a numerical variable sampled over a time series

data("Nile")  # load the datset about water levels through time

Nile # reports the data in the console

plot(Nile, type="l") #basic

plot(Nile, type="l", las = 1, col = "blue", ylab = "Water Flow of Nile River", 
     xlab = "Year")


##  Strip charts

data("chickwts") #bring in the built-in dataset
chickwts # if you want to see the data in the console

stripchart(weight ~ feed, data = chickwts, method = "jitter", las = 2, vertical = TRUE)

stripchart(weight ~ feed, data = chickwts, xlab = "Type of feed", ylab = "Weight (g)", 
           col = "firebrick", las = 2, method = "jitter", vertical = TRUE) 


##  Boxplots

boxplot(weight ~ feed, data = chickwts) #basic

boxplot(weight ~ feed, data = chickwts, las = 2, col = "goldenrod")

