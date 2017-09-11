library(Biobase)
source("functions.R")

getFeatureRatio <- function(pair, eMatrix) {
    fX = pair[1]
    fY = pair[2]
    ratio = eMatrix[fX,] / eMatrix[fY,]
    maxValue = max(ratio[!is.infinite(ratio)])
    ratio[is.infinite(ratio)] = maxValue
    ratio
}
args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
objectName = args[2]
outputFile = args[3]

load(inputFile)
eSet = get(objectName)

eMatrix = exprs(eSet)

pairs = expand.grid(x = rownames(eMatrix), y = rownames(eMatrix), 
    stringsAsFactors = FALSE)
pairs = as.matrix(pairs[pairs$x != pairs$y,])

expandedMatrix = t(apply(pairs, 1, getFeatureRatio, eMatrix))
featureTable = fData(eSet)

featureNames = apply(pairs, 1, paste, collapse = "/")
expandedFeatureTable = data.frame(name = featureNames,
    type = "ratio", 
    description = sprintf("ratio between %s", 
        apply(pairs, 1, paste, collapse = " and ")),
    correctionFormula = NA, stringsAsFactors = FALSE, 
    row.names = featureNames)
rownames(expandedMatrix) <- featureNames
newFeatureTable = rbind(featureTable, expandedFeatureTable)
newEMatrix = rbind(eMatrix, expandedMatrix)

eSet = ExpressionSet(newEMatrix, phenoData = eSet@phenoData, 
    featureData = aDataFrame(newFeatureTable))
assign(objectName, eSet)
save(list = objectName, file = outputFile)
