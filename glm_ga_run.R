library(glmulti)
library(Biobase)
source("functions.R")

args = commandArgs(trailingOnly = TRUE)
outcomeVariable = "professional.diagnosis"
outputFile = args[2]

load("data/training_expanded.Rda")
load("data/topTables.Rda")

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

gaRun = glmulti(outcomeVariable, featureNames[1:30], level=1, data = inputData,
    method="g", family = family, crit = "aicc", marginality = TRUE, 
    report = FALSE)

save(gaRun, file = outputFile)
