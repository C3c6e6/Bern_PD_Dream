library(Biobase)
library(ClusterTools)
library(ggplot2)
source("functions.R")

featuresPerDiameterCutoff <- function(result, dRange) {
    clusinfo = as.data.frame(result$clusinfo)
    clusinfo = clusinfo[clusinfo$size > 1,]
    clusinfo = clusinfo[order(clusinfo$max_diss),]
    output = data.frame(d = dRange, 
        nReduced = sapply(dRange, 
            function(d) sum(clusinfo$size[clusinfo$max_diss <= d] - 1))
    )
    output
}

featureReductionData <- function(kMedoids, dRange = (1:50)/100) {
    allData = lapply(kMedoids$allResults, featuresPerDiameterCutoff, dRange)
    output = do.call(rbind, allData)
    output$k = rep(kMedoids$kRange, each=length(dRange))
    output
}


args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]

load(inputFile)
load("data/topTables.Rda")

maxD = as.numeric(readLines("params/maxD"))
pdf("results/plots/reduce_features.pdf", width=12, height=6)



featureTable = fData(training)
featureTypes = unique(featureTable$type)

eMatrix = exprs(training)
distances = corDist(eMatrix)
maxK = nrow(eMatrix) - 1
kMedoids = optimalKMedoids(distances, 2:maxK)
clusters = kMedoids$optimalResult$clustering
plot(hclust(distances), cex=0.4)
reduction = featureReductionData(kMedoids)
p = ggplot(data=reduction, aes(x=k, y=d, fill=nReduced)) + geom_raster()
print(p)
reductionMax = tapply(reduction$nReduced, reduction$d, max)
maxTable = data.frame(nReduced = reductionMax, 
    d = as.numeric(names(reductionMax)))
p = ggplot(data=maxTable, aes(x=d, y=nReduced)) + geom_point() + geom_line()
print(p)
reduction = reduction[reduction$d <= maxD,]
k = reduction$k[which.max(reduction$nReduced)]
kString = sprintf("k= %d", k)
selectedFeatures = kMedoids$allResults[[kString]]$medoids
dev.off()
message(length(selectedFeatures), " features selected.")

top = topTables$professional.diagnosis
top = top[selectedFeatures,]
featureOrder = order(top$padj, top$p)
selectedFeatures = selectedFeatures[featureOrder]
training = training[selectedFeatures,]

save(training, file = "data/training_reduced.Rda")