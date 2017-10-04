library(Biobase)
library(MASS)
library(glmulti)

args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
objectName = args[2]
outputFile = args[3]

load(inputFile)
eSet = get(objectName)

getModelData <- function(featureValues, pheno) {
    modelData = pheno
    modelData$feature = featureValues
    modelData
}

getFormula <- function(modelData) {
    glmultiRun = glmulti("feature", c("age", "gender", "phoneinf_event"), 
        modelData, method="h", level=2, marginality = TRUE, report = FALSE)
    paste(summary(glmultiRun)$bestmodel, collapse = "")
}

correctValues <- function(modelData, formulaString) {
    modelFormula = as.formula(formulaString)
    if (length(all.vars(modelFormula)) == 1) {
        modelData$feature
    } else {
        model = rlm(modelFormula, modelData)
        model$residuals
    }
}

eMatrix = exprs(eSet)
message("building model data frames...")
modelDataList <- apply(eMatrix, 1, getModelData, pData(eSet))
message("determining best models...")
formulae = sapply(modelDataList, getFormula)
message("fitting models...")
correctedValues = mapply(correctValues, modelDataList, formulae,
    SIMPLIFY = FALSE)
message("Creating new matrix...")
correctedMatrix = do.call(rbind, correctedValues)

exprs(eSet) <- correctedMatrix
fData(eSet)$correctionFormula = formulae

assign(objectName, eSet)
save(list = objectName, file = outputFile)
