library(glmulti)
library(Biobase)
source("functions.R")

args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
outcomeVariable = args[2]
outputFile = args[3]

load(inputFile)
nFeaturesExhaustive = as.numeric(readLines("params/nFeaturesExhaustive"))

pheno = pData(training)
eMatrix = exprs(training)
inputData = cbind(pheno, t(eMatrix))#, t(reciproqueMatrix))
family = if (is.numeric(inputData[[outcomeVariable]])) "gaussian" else 
    "binomial"

featureNames = rownames(training)
n = min(nFeaturesExhaustive, length(featureNames))
includedFeatures = featureNames[1:n]

glmultiRun = glmulti(outcomeVariable, includedFeatures, level=1, data = inputData,
    method="h", family = family, crit = "aicc", marginality = TRUE, 
    report = TRUE)

formula = as.formula(paste(summary(glmultiRun)$bestmodel, collapse=""))

model = glm(formula, data = inputData)
save(model, file = outputFile)
