library(Biobase)
library(parallel)
options(mc.cores = 4)

rangeGap <- function(values, valueFactor, q = c(0.05, 0.95)) {
    factorMedians = tapply(values, valueFactor, median)
    quantiles = tapply(values, valueFactor, quantile, q)
    maxLevel = names(which.max(factorMedians))
    minLevel = names(which.min(factorMedians))
    distance = quantiles[[maxLevel]][1] - quantiles[[minLevel]][2]
    globalQuantiles = quantile(values, q)
    norm = globalQuantiles[2] - globalQuantiles[1]
    gap = distance/norm
    names(gap) = NULL
    gap
}

oneWayTest <- function(values, variableFactor) {
    testData = data.frame(v = values, f = variableFactor)
    output = oneway.test(v ~ f, data = testData)
    output
}

kruskalTopTable <- function(variable, eSet) {
    message(variable)
    eMatrix = exprs(eSet)
    variableFactor = factor(pData(eSet)[[variable]])
    topTable = fData(eSet)
    topTable$variable = variable
    tests = apply(eMatrix, 1, oneWayTest, variableFactor)
    topTable$p = sapply(tests, function(x) x$p.value)
    topTable$padj = p.adjust(topTable$p, method="fdr")
    topTable$rangeGap = apply(eMatrix, 1, rangeGap, variableFactor)
    topTable = topTable[order(topTable$padj, topTable$p),]
    topTable
}

correlationTopTable <- function(variable, eSet) {
    message(variable)
    eMatrix = exprs(eSet)
    variableValues = pData(eSet)[[variable]] + runif(ncol(eSet), max=1e-6)
    topTable = fData(eSet)
    topTable$variable = variable
    tests = apply(eMatrix, 1, cor.test, variableValues, method="spearman")
    topTable$r = sapply(tests, function(x) x$estimate)
    topTable$p = sapply(tests, function(x) x$p.value)
    topTable$padj = p.adjust(topTable$p, method="fdr")
    topTable = topTable[order(topTable$padj, topTable$p),]
    topTable
}

outcomes = c(readLines("params/outcomes"))
args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
load(inputFile)

dataset = training
eMatrix = exprs(dataset)
numericOutcomes = sapply(pData(dataset)[,outcomes], is.numeric)
topTables = c(
    mclapply(outcomes[!numericOutcomes], kruskalTopTable, dataset),
    mclapply(outcomes[numericOutcomes], correlationTopTable, dataset) )
names(topTables) <- c(outcomes[!numericOutcomes], 
    outcomes[numericOutcomes])

save(topTables, file="data/topTables.Rda")
