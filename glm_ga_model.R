library(glmulti)
library(SetTools)
library(Biobase)
source("functions.R")

args = commandArgs(trailingOnly = TRUE)
outputFile = args[1]
outcomeVariable = args[2]
inputFiles = args[3:length(args)]

runs = list()
for (fileName in inputFiles) {
    load(fileName)
    runs[[fileName]] = gaRun
}

load("data/training_expanded.Rda")

pheno = pData(training)
response = pheno[[outcomeVariable]]
sampleSelection = !is.na(response)
response = response[sampleSelection]
inputData = as.data.frame(t(exprs(training)[,sampleSelection]))
inputData[[outcomeVariable]] = response

if (is.numeric(response)) {
    family = "gaussian"
} else {
    family = "binomial"
} 

glmultiRun = consensus(runs)
formulaVariables = lapply(lapply(glmultiRun@formulas, as.formula), all.vars)
allTerms = unique(unlist(formulaVariables)) %d% outcomeVariable
includedTerms = lapply(allTerms, 
    function(x) sapply(formulaVariables, function(y) x %in% y))
termWeights = sapply(includedTerms, 
    function(x) sum(summary(glmultiRun)$modelweights[x]))
selectedTerms = allTerms[termWeights > 0.8]
if (length(selectedTerms) > 0) {
    formula = sprintf("%s ~ 1 + %s", outcomeVariable, 
        paste(selectedTerms, collapse = " + "))
} else {
    formula = sprintf("%s ~ 1", outcomeVariable)
}
message("selected model: ", formula)
model = glm(formula, data=inputData, family=family)
save(model, file = outputFile)
