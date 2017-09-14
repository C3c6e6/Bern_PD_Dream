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

load("data/features_clip_v4.Rda")

rownames(featureTab) <- featureTab$recordId_event
colnames(featureTab) <- gsub("u?Accel_Outbnd_", "", 
    make.names(colnames(featureTab)))
featureTab$age = as.numeric(featureTab$age)
featureTab$are.caretaker = as.logical(featureTab$are.caretaker)
featureTab$deep.brain.stimulation = 
    as.logical(featureTab$deep.brain.stimulation)
featureTab$diagnosis.year = as.numeric(featureTab$diagnosis.year)
featureTab$professional.diagnosis = 
    as.logical(featureTab$professional.diagnosis)
featureTab$home.usage = as.logical(featureTab$home.usage)
featureTab$last.smoked = as.numeric(featureTab$last.smoked)
featureTab$medical.usage = as.logical(featureTab$medical.usage)
featureTab$medical.usage.yesterday = 
    as.logical(featureTab$medical.usage.yesterday)
featureTab$medication.start.year = as.numeric(featureTab$medication.start.year)
featureTab$medication.start.year[featureTab$medication.start.year == 0] = NA
featureTab$onset.year = as.numeric(featureTab$onset.year)
featureTab$packs.per.day = as.numeric(featureTab$packs.per.day)
featureTab$past.participation = as.logical(featureTab$past.participation)
featureTab$phone.usage = as.logical(featureTab$phone.usage)
featureTab$smoked = as.logical(featureTab$smoked)
featureTab$surgery = as.logical(featureTab$surgery)
featureTab$video.usage = as.logical(featureTab$video.usage)
featureTab$years.smoking =as.numeric(featureTab$years.smoking)

featureTab$medTimepoint = sub(".+don't.+", "none", featureTab$medTimepoint)
featureTab$medTimepoint = sub(".+(before|after).+", "\\1", 
    featureTab$medTimepoint)

naRecords = is.na(featureTab$medTimepoint) | 
    is.na(featureTab$deep.brain.stimulation) | 
    is.na(featureTab$professional.diagnosis)
featureTab = featureTab[!naRecords,]
exclude = !featureTab$professional.diagnosis != 
    (featureTab$medTimepoint == "none") 
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

trainingObservations =
    rownames(featureTab[!duplicated(featureTab$healthCode),])
testObservations = rownames(featureTab) %d% trainingObservations
training = table2ExpressionSet(featureTab[trainingObservations,],
    motionFeatures)
test = table2ExpressionSet(featureTab[testObservations,],
    motionFeatures)

commonFeatures = intersect(rownames(training), rownames(test))
training = training[commonFeatures,]
test = test[commonFeatures,]

save(training, file="data/training.Rda")
save(test, file="data/test.Rda")
