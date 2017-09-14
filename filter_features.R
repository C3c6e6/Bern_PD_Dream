library(Biobase)

args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
objectName = args[2]
outputFile = args[3]

load("data/topTables.Rda")
nFeatures = as.numeric(readLines("params/nFeatures"))
load(inputFile)
eSet = get(objectName)

sortedFeatures = rownames(topTables$professional.diagnosis)
eSet = eSet[sortedFeatures[1:nFeatures],]

assign(objectName, eSet)
save(list = objectName, file = outputFile)
