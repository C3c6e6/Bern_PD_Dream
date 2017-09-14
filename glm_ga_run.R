library(glmulti)
library(Biobase)
source("functions.R")

args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
outcomeVariable = args[2]
outputFile = args[3]

load(inputFile)

pheno = pData(training)
eMatrix = exprs(training)
inputData = cbind(pheno, t(eMatrix))
family = if (is.numeric(inputData[[outcomeVariable]])) "gaussian" else 
    "binomial"
featureNames = rownames(training)

gaRun = glmulti(outcomeVariable, featureNames[1:30], level=1, data = inputData,
    method="g", family = family, crit = "aicc", marginality = TRUE, 
    report = FALSE)

save(gaRun, file = outputFile)
