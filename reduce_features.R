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

load("data/training_corrected.Rda")
maxD = as.numeric(readLines("params/maxD"))
eMatrix = exprs(training)

distances = corDist(eMatrix)
maxK = nrow(eMatrix) - 1

kMedoids = optimalKMedoids(distances, 2:maxK)
clusters = kMedoids$optimalResult$clustering

pdf("results/plots/reduce_features.pdf", width=12, height=6)
plot(hclust(distances), cex=0.4)

reduction = featureReductionData(kMedoids)
ggplot(data=reduction, aes(x=k, y=d, fill=nReduced)) + geom_raster()
reductionMax = tapply(reduction$nReduced, reduction$d, max)
maxTable = data.frame(nReduced = reductionMax, 
    d = as.numeric(names(reductionMax)))
ggplot(data=maxTable, aes(x=d, y=nReduced)) + geom_point() + geom_line()
dev.off()

reduction = reduction[reduction$d <= maxD,]
k = reduction$k[which.max(reduction$nReduced)]
kString = sprintf("k= %d", k)
selectedFeatures = kMedoids$allResults[[kString]]$medoids
message("Selected features: ")
writeLines(selectedFeatures, con = stderr())
training = training[selectedFeatures,]
save(training, file = "data/training_reduced.Rda")