library(MontgomeryDAE)

# Section 2.4.1 - 2.4.2 - Cement example

Table2.1 #show table of cement data and do some basic stats!
mean(Table2.1$ModifiedMortar)
var(Table2.1$ModifiedMortar)
mean(Table2.1$UnmodifiedMortar)
var(Table2.1$UnmodifiedMortar)

qt(0.025,18) #quickly demonstrate t-values from Table II (lower)
qt(0.975,18) #quickly demonstrate t-values from Table II (upper)
t.test(Table2.1$ModifiedMortar, Table2.1$UnmodifiedMortar) #Welch two sample t-test
t.test(Table2.1$ModifiedMortar, Table2.1$UnmodifiedMortar,var.equal=TRUE) #Traditional t-test
t.test(Table2.1$ModifiedMortar, Table2.1$UnmodifiedMortar, var.equal = TRUE,conf.level = 0.99) #Conf interval to 99, try others%!!
t.test(Table2.1$ModifiedMortar, Table2.1$UnmodifiedMortar, var.equal = TRUE,conf.level = 0.95,alternative="greater") # this one is stupid because we can see that modified is stronger than unmodified!
t.test(Table2.1$ModifiedMortar, Table2.1$UnmodifiedMortar, var.equal = TRUE,conf.level = 0.95,alternative="less")

# Section 2.4.3 Choice of Sample size
library(pwr)
pwr.t.test(n=10, d=2, sig.level=0.05, type='two.sample', alternative='two.sided') # 98.8% power!
pwr.t.test(n=NULL, d=2, sig.level=0.05, type='two.sample', alternative='two.sided',power=0.8) # how many for 90% power?
pwr.t.test(n=NULL, d=2, sig.level=0.05, type='two.sample', alternative='two.sided',power=0.9) # how many for 90% power?
# section 2.4.4. Different variances 
Table2.3
t.test(Table2.3$Nerve,Table2.3$Muscle,alternative="greater")
t.test(Table2.3$Nerve,Table2.3$Muscle,alternative="greater",conf.level = 0.5)
t.test(Table2.3$Nerve,Table2.3$Muscle,alternative="greater",conf.level = 0.31) # Go crazy, try to find >2000

# Section 2.4.6 (skip 2.4.5)) Single mean to a specified value
library(BSDA) # R doesn't have a built in Z test because it always does t-test!
fabric_data <- c(202, 210, 218, 226) # Generate random data with n = 4 and mean = 214
mean(fabric_data)
z.test(x=fabric_data,mu=200,sigma.x=10,alternative="greater")
t.test(x=fabric_data,mu=200,sigma.x=10,alternative="greater")
# Section 2.5 - Paired comparisons
Table2.6
t.test(Table2.6$Tip1,Table2.6$Tip2) #not paired
t.test(Table2.6$Tip1,Table2.6$Tip2,paired=TRUE) #paired

#import BHH shoe example
tab0305 <- read.csv("course-files/BHHdata/tab0305.dat", sep="")
tab0305
t.test(tab0305$matA,tab0305$matB)
t.test(tab0305$matA,tab0305$matB,paired=TRUE)
t.test(tab0305$matA,tab0305$matB,paired=TRUE,alternative="less")
#Section 2.6 - Inferences About the Variances of Normal Distributions

#Battery example
(chi2_val <- (6)*(29^2)/(20^2)) #Calculate Chi Squared value for Battery example
pchisq(chi2_val,6) #Battery example with no lower tail
pchisq(chi2_val,6, lower.tail = FALSE) #Battery example with only upper tail!
(chi2_val <- (14)*(29^2)/(20^2)) #Calculate Chi Squared value for Battery example
pchisq(chi2_val,14, lower.tail = FALSE) #Battery example with only upper tail!
(chi2_val <- (29)*(29^2)/(20^2)) #Calculate Chi Squared value for Battery example
pchisq(chi2_val,29, lower.tail = FALSE) #Battery example with only upper tail!

#Example 2.3 - variability of test equipment
qf(0.05,11,9,lower.tail= FALSE) # Example 2.3, 5% level of significance (alpha)

# Create dummy data for R
sample1 <- scale(1:12) * sqrt(14.5)  # n1 = 10, s1^2 = 14.5
sample2 <- scale(1:10) * sqrt(10.8)  # n2 = 12, s2^2 = 10.8

# Demonstrate mean and variance are as published
nrow(sample1)
var(sample1)
mean(sample1)
nrow(sample2)
var(sample2)
mean(sample2)

var.test(sample1,sample2) #generate F statistics
