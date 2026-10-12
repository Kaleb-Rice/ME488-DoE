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