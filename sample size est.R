# Calculating needed sample size for studies

# bring in the "Freshman 15" example
frosh <- read.csv(url("https://www.dropbox.com/s/emerssvl2w7gkog/frosh.csv?dl=1"))
frosh$difference <- frosh$april-frosh$sept
t.test(frosh$difference, mu = 15) 
(5.251557 - 1.794597)/2 # calculate the half-width of the 95% CI
sd(frosh$difference)

install.packages("pwr") # only need to do this once to install

library(pwr) # activate this package after installation


## Calculating sample size and power for one-sample and paired t tests

delta <- 1.72848  # half-width of the CI: a difference in means that would be meaningful
sigma <- 6.975644  # the (estimated) SD of the sample
d <- delta/sigma

pwr.t.test(d=d, sig.level=.05, power = .90)


## Calculating sample size and power for two-sample t tests

delta <- 3  # half-width of the CI: a difference in means that would be meaningful
sigma <- 28.4  # the (estimated) variance of the sample
d <- delta/sigma # or can fix to a desired difference

pwr.t2n.test(d=0.5, sig.level= 0.05, n1=28, n2=35, power = 0.8, alternative = "greater") # one-tailed

# Note that EITHER one of the sample sizes or the significance level or the power 
# has to be stated as NULL to let the other parts of the equation move



