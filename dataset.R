library(Biobase)
library(SetTools)
library(e1071)
source("functions.R")

table2ExpressionSet <- function(featureTab, motionFeatures) {
    metaFeatures = colnames(featureTab) %d% motionFeatures
    eMatrix = t(featureTab[,motionFeatures])
    pheno = featureTab[,metaFeatures]
    pheno$gender[!pheno$gender %in% c("Male", "Female")] = NA
    pheno$createdOn_event = as.Date(pheno$createdOn_event)
    pheno$diagnosis.year = as.Date(ISOdate(pheno$diagnosis.year, 1, 1))
    pheno$onset.year = as.Date(ISOdate(pheno$onset.year, 1, 1))
    pheno$timeSinceOnset = as.numeric(pheno$createdOn_event - pheno$onset.year)
    features = data.frame(name = rownames(eMatrix), type = "original", 
        description = "original feature", row.names = rownames(eMatrix),
        stringsAsFactors = FALSE)
    logFeatures = grepl("^log_", rownames(eMatrix))
    sigmoidFeatures = grepl("^sigm_", rownames(eMatrix))
    features$type[logFeatures] = "log-transformed"
    features$type[sigmoidFeatures] = "sigmoid-transformed"
    features$description[logFeatures] = sprintf("pseudo-log transform of %s",
        sub("^log_", "", rownames(eMatrix)[logFeatures]))
    features$description[sigmoidFeatures] = sprintf("sigmoid transform of %s",
         sub("^sigm_", "", rownames(eMatrix)[sigmoidFeatures]))
    training = ExpressionSet(eMatrix, phenoData = aDataFrame(pheno),
        featureData = aDataFrame(features))
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

motionFeatures = colnames(featureTab)[38:ncol(featureTab)]
for (feature in motionFeatures) {
    transforms = list()
    transforms[[feature]] = featureTab[[feature]]
    if (all(is.na(transforms[[feature]]))) {
        next
    }
    featureTab[[feature]] = NULL
    logName = sprintf("log_%s", feature)
    transforms[[logName]] = logTransform(transforms[[feature]])
    sigmoidName = sprintf("sigm_%s", feature)
    transforms[[sigmoidName]] = sigmoid(transforms[[feature]])
    kurtosisValues = sapply(transforms, kurtosis, na.rm = TRUE)
    bestTransform = which.min(kurtosisValues)
    bestName = names(transforms)[bestTransform]
    featureTab[[bestName]] = transforms[[bestName]]
}
motionFeatures = colnames(featureTab)[38:ncol(featureTab)] #update motionFeatures

training = table2ExpressionSet(featureTab[!duplicated(featureTab$healthCode),],
    motionFeatures)
test = table2ExpressionSet(featureTab[duplicated(featureTab$healthCode),],
    motionFeatures)

commonFeatures = intersect(rownames(training), rownames(test))
training = training[commonFeatures,]
test = test[commonFeatures,]

save(training, file="data/training.Rda")
save(test, file="data/test.Rda")
