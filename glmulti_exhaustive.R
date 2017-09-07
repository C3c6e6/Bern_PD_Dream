library(glmulti)
library(Biobase)
source("functions.R")

args = commandArgs(trailingOnly = TRUE)
outcomeVariable = "professional.diagnosis"
outputFile = args[2]

load("data/training_reduced.Rda")
load("data/topTables.Rda")
nFeaturesExhaustive = as.numeric(readLines("params/nFeaturesExhaustive"))

pheno = pData(training)
eMatrix = exprs(training)
#reciproqueMatrix = 1/eMatrix
#rownames(reciproqueMatrix) = paste0("rec_", rownames(eMatrix))
inputData = cbind(pheno, t(eMatrix))#, t(reciproqueMatrix))
family = if (is.numeric(inputData[[outcomeVariable]])) "gaussian" else 
    "binomial"
#featureNames = c(rownames(eMatrix))#, rownames(reciproqueMatrix))
varTopTable = topTables[[outcomeVariable]][rownames(training),]
featureNames = rownames(training)[order(varTopTable$padj, varTopTable$p)]
includedFeatures = featureNames[1:nFeaturesExhaustive]

glmultiRun = glmulti(outcomeVariable, includedFeatures, level=1, data = inputData,
    method="h", family = family, crit = "aicc", marginality = TRUE, 
    report = TRUE)

formula = as.formula(paste(summary(glmultiRun)$bestmodel, collapse=""))

model = glm(formula, data = inputData)
save(model, file = "data/glm_exhaustive.Rda")
