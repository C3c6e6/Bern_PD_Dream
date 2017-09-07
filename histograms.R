library(Biobase)
library(ggplot2)
library(reshape2)
load("data/training.Rda")

eMatrix = exprs(training)

plotData = melt(eMatrix, varnames = c("feature", "observation"))
pdf("results/plots/histograms.pdf", width = 20, height = 20)
ggplot(data=plotData, aes(x=value)) + geom_histogram(bins = 50) + 
    facet_wrap(~feature, scales = "free")
dev.off()
