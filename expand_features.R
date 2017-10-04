library(Biobase)
library(e1071)
library(scales)
source("functions.R")

getFeatureRatio <- function(pair, eMatrix) {
    fX = pair[1]
    fY = pair[2]
    ratio = eMatrix[fX,] / eMatrix[fY,]
    maxValue = max(ratio[!is.infinite(ratio)])
    ratio[is.infinite(ratio)] = maxValue
    transforms = list()
    transforms$log = logTransform(ratio)
    transforms$none = ratio
    transforms$sigmoid = sigmoid(ratio)
    kurtosisValues = sapply(transforms, kurtosis)
    bestTransform = which.min(kurtosisValues)
    transforms[[bestTransform]]
}

polyNFeature <- function(x, p) x^p

getExpandedMatrix <- function(p, eMatrix) {
    expandedMatrix = t(apply(eMatrix, 1, polyNFeature, p))
}

getExpandedFeatureTable <- function(expandedMatrix, p) {
    featureNames = sprintf("%s_%d", rownames(eMatrix), p)
    expandedFeatureTable = data.frame(name = featureNames,
        type = "polynomial", 
        description = sprintf("%s power of %s", ordinal(p), rownames(eMatrix)),
        correctionFormula = NA, stringsAsFactors = FALSE, 
        row.names = featureNames)
}

args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
objectName = args[2]
outputFile = args[3]

load(inputFile)
eSet = get(objectName)

eMatrix = exprs(eSet)

featureTable = fData(eSet)

polyRange = as.integer(readLines("params/polyRange"))

expandedMatrices = lapply(polyRange, getExpandedMatrix, eMatrix)
expandedMatrix = do.call(rbind, expandedMatrices)
expandedFeatureTable <- do.call(rbind, 
    mapply(getExpandedFeatureTable, expandedMatrices, polyRange, SIMPLIFY = FALSE))

rownames(expandedMatrix) <- rownames(expandedFeatureTable)
newFeatureTable = rbind(featureTable, expandedFeatureTable)
newEMatrix = rbind(eMatrix, expandedMatrix)

eSet = ExpressionSet(newEMatrix, phenoData = eSet@phenoData, 
    featureData = aDataFrame(newFeatureTable))
assign(objectName, eSet)
save(list = objectName, file = outputFile)
