# Created by Kaleb Rice 2026

# This document is has only been used as a place to develop my solutions and is
# only saved for reference purposes.
# Please refer to the associated Quarto document for finished work.



# Problem 2.1
variableName <- c("Y")
nn <- c(9)
meanResult <- c(19.96)
SEMeanResult <- c(NA)
stdDevResult <- c(3.12)
varianceResult <- c(NA)
minResult <- c(15.94)
maxResult <- c(27.16)

# Data frame
sampData <- data.frame(aa = variableName, 
                      bb = nn, 
                      cc = meanResult, 
                      dd = SEMeanResult,
                      ee = stdDevResult,
                      ff = varianceResult,
                      gg = minResult,
                      hh = maxResult)
colnames(sampData) <- c("Variable", "N", "Mean", "SE Mean", "Std. Dev.", "Variance", "Minimum", "Maximum")
sampData


# Problem 2.7
p7Data = c(9.37, 13.04, 11.69, 8.21, 11.18, 10.41, 13.15, 11.51, 13.21, 7.75)
sprintf("Mean: %.3f", mean(p7Data))
sprintf("Standard Deviation: %.3f", sd(p7Data))
qqnorm(p7Data)
qqline(p7Data, col = "red", lwd = 2)
hist(p7Data, xlab = "Value", col = "gray", border = "black",
     main = "Histogram of Problem 2.7 Data",
     xlim = c(0, 20), ylim = c(0, 5), breaks = 5)
t.test(p7Data, mu = 10) # Defaults to two-sided 95% confidence test