library(Biobase)
library(SetTools)
source("functions.R")

table2ExpressionSet <- function(featureTab) {
    motionFeatures = colnames(featureTab)[38:ncol(featureTab)]
    metaFeatures = colnames(featureTab) %d% motionFeatures
    eMatrix = t(featureTab[,motionFeatures])
    pheno = featureTab[,metaFeatures]
    pheno$gender[!pheno$gender %in% c("Male", "Female")] = NA
    pheno$createdOn_event = as.Date(pheno$createdOn_event)
    pheno$diagnosis.year = as.Date(ISOdate(pheno$diagnosis.year, 1, 1))
    pheno$onset.year = as.Date(ISOdate(pheno$onset.year, 1, 1))
    pheno$timeSinceOnset = as.numeric(pheno$createdOn_event - pheno$onset.year)
    training = ExpressionSet(eMatrix, phenoData = aDataFrame(pheno))
    naMatrix = t(apply(exprs(training), 1, is.na))
    allNAfeatures = apply(naMatrix, 1, all)
    allNAobservations = apply(naMatrix, 1, all)
    training = training[!allNAfeatures, !allNAobservations]
    naMatrix = t(apply(exprs(training), 1, is.na))
    naFeatures = apply(naMatrix, 1, any)
    naObservations = apply(naMatrix, 2, any)
    pheno = pData(training)
    phenoExclude = is.na(pheno$gender) | is.na(pheno$age) | 
        is.na(pheno$professional.diagnosis)
    training = training[,!naObservations & !phenoExclude]
    training
}

load("data/features_clip_v2.Rda")

rownames(featureTab) <- featureTab$recordId_event
colnames(featureTab) <- gsub("uAccel_Outbnd_", "", 
    make.names(colnames(featureTab)))
featureTab$medTimepoint = sub(".+don't.+", "none", featureTab$medTimepoint)
featureTab$medTimepoint = sub(".+(before|after).+", "\\1", 
    featureTab$medTimepoint)
featureTab = 
    featureTab[featureTab$medTimepoint %in% c("before", "none"),]
featureTab = featureTab[!featureTab$deep.brain.stimulation,]
exclude = featureTab$professional.diagnosis != 
    (featureTab$medTimepoint == "before") & is.na(featureTab$health.history)
featureTab = featureTab[!exclude,]

featureTab$phoneInf_event = sub("\\s*\\(.+", "", featureTab$phoneInf_event)

logFeatures = readLines("params/logFeatures")
for (feature in logFeatures) {
    newName = sprintf("log%s", feature)
    featureTab[[newName]] = logTransform(featureTab[[feature]])
    featureTab[[feature]] = NULL
}


training = table2ExpressionSet(featureTab[!duplicated(featureTab$healthCode),])
test = table2ExpressionSet(featureTab[duplicated(featureTab$healthCode),])

save(training, file="data/training.Rda")
save(test, file="data/test.Rda")
