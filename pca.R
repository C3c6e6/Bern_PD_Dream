# TODO: Add comment
# 
# Author: cesim
###############################################################################
library(ggplot2)
library(Biobase)
source("functions.R")
args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
outputFile = args[2]
colorVar = args[3]
load(inputFile)

pdf(outputFile, width = 7, height = 8)
name = "training"
eSet = training
features = fData(eSet)
pca = prcomp(t(exprs(eSet)), scale. = TRUE)
pheno = pData(eSet)
plotData = cbind(pheno, pca$x[,1:3])
sizeRange = c(0.1, 1)
plotData = plotData[plotData$gender %in% c("Male", "Female"),]
k = 2.5
selection = !isOutlier(plotData$PC1, k) & !isOutlier(plotData$PC2, k) & 
    !isOutlier(plotData$PC3, k)
plotData = plotData[selection,]
plot(pca, main = name)
p = ggplot(data=plotData, 
    aes_string(x="PC1", y="PC2", color = colorVar, shape="gender", 
        size = "age")) +
    geom_point(alpha = 0.7) + ggtitle(name) + 
    scale_size_continuous(range=sizeRange) +
    theme(legend.position = "bottom") #+ facet_wrap(~professional.diagnosis)
print(p)
p = ggplot(data=plotData, 
    aes_string(x="PC1", y="PC3", color = colorVar, shape="gender", 
        size = "age")) +
    geom_point(alpha = 0.7) + ggtitle(name) + 
    scale_size_continuous(range=sizeRange) +
    theme(legend.position = "bottom") #+ facet_wrap(~professional.diagnosis)
print(p)
p = ggplot(data=plotData, 
    aes_string(x="PC2", y="PC3", color = colorVar, shape="gender", 
        size = "age")) +
    geom_point(alpha = 0.7) + ggtitle(name) + 
    scale_size_continuous(range=sizeRange) +
    theme(legend.position = "bottom") #+ facet_wrap(~professional.diagnosis)
print(p)
dev.off()

