library(Biobase)
library(parallel)
options(mc.cores = 4)

kruskalTopTable <- function(variable, eSet) {
    message(variable)
    eMatrix = exprs(eSet)
    variableFactor = factor(pData(eSet)[[variable]])
    topTable = fData(eSet)
    topTable$variable = variable
    tests = apply(eMatrix, 1, kruskal.test, variableFactor)
    topTable$p = sapply(tests, function(x) x$p.value)
    topTable$padj = p.adjust(topTable$p, method="fdr")
    topTable = topTable[order(topTable$padj, topTable$p),]
    topTable
}

correlationTopTable <- function(variable, eSet) {
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
