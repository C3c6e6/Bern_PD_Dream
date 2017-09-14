library(mixOmics)
library(Biobase)
source("functions.R")

bestMinIndex <- function(values) {
    absoluteMin = min(values)
    sDev = sd(values)
    min(which(values <= absoluteMin + sDev))
}

getNCompAndDistance <- function(performance) {
    errorRates = performance$error.rate$overall
    bestIndices = apply(errorRates, 2, bestMinIndex)
    lowestValues = sapply(names(bestIndices), 
        function(n) errorRates[bestIndices[n],n])
    bestDistance = names(which.min(lowestValues))
    nComp = bestIndices[bestDistance]
    list(distance=bestDistance, ncomp=nComp)
}

getNComp <- function(performance) {
    list(ncomp = bestMinIndex(-performance$Q2.total))
}

evaluateX <- function(x, previousX, modelFunction, expression, response, ncomp, 
    params) {
    model = modelFunction(expression, response, ncomp = ncomp, 
        keepX = c(previousX,x), scale = TRUE)
    performance = perf(model, validation="Mfold", dist = params$distance, 
        progressBar=FALSE, folds = folds)
    if (!is.null(performance$error.rate)) {
        performance$error.rate
    } else {
        -performance$MSEP
    }
}

args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
outcomeVariable = args[2]
outputFile = sprintf("data/glm_spls_%s_model.Rda", outcomeVariable)
load(inputFile)

pheno = pData(training)
response = pheno[[outcomeVariable]]
sampleSelection = !is.na(response)
response = response[sampleSelection]
inputData = t(exprs(training)[,sampleSelection])
if (is.numeric(response)) {
    tuneFunction = pls
    sparseFunction = spls
    family = "gaussian"
    paramFunction = getNComp
} else {
    tuneFunction = plsda
    sparseFunction = splsda
    family = "binomial"
    paramFunction = getNCompAndDistance
}
message(family)

maxX = min(nrow(training), floor(ncol(training)/2))
xRange = 1:maxX
maxComp = 10
folds = 10
exploreModel = tuneFunction(inputData, response, ncomp = maxComp)
performance = perf(exploreModel, validation = "Mfold", folds = folds, 
    progressBar = TRUE)
params = paramFunction(performance)
nComp = params$ncomp
distance = params$distance
message("Best distance: ", distance, "; ncomp: ", nComp)

keepX = c()
for (n in 1:nComp) {
    errorRates = lapply(xRange, evaluateX, keepX, sparseFunction, inputData, 
        response, n, params)
    errorTable = do.call(rbind, lapply(errorRates, function(x) 
        if (class(x) == "list") t(x$overall) else x))
    optimalX = xRange[bestMinIndex(errorTable[,n])]
    message("best X for comp ", n, ": ", optimalX)
    keepX = c(keepX, optimalX)
}
finalModel = sparseFunction(inputData, response, keepX=keepX, ncomp=nComp)
selectedFeatures = if (!"splsda" %in% class(finalModel)) 
    selectVar(finalModel)$X$name else selectVar(finalModel)$name

if (length(selectedFeatures) > 0) {
    if (length(selectedFeatures) == 1) {
        modelData = pheno[sampleSelection,]
        modelData[[selectedFeatures]] = inputData[,selectedFeatures]
    } else {
        modelData = cbind(pheno[sampleSelection,], inputData[,selectedFeatures])
    }
    formulaString = sprintf("%s ~ 1 + %s", outcomeVariable, 
        paste(selectedFeatures, collapse = " + "))
    message(formulaString)
    model = glm(as.formula(formulaString), data=modelData, family = family)
} else {
    message("No features selected")
    model = getNullModel(outcomeVariable, pheno[sampleSelection,])
}
save(model, file = outputFile)
