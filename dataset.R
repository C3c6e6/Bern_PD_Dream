library(Biobase)
library(SetTools)
library(e1071)
source("functions.R")

table2ExpressionSet <- function(featureTab, motionFeatures) {
    metaFeatures = colnames(featureTab) %d% motionFeatures
    eMatrix = t(featureTab[,motionFeatures])
    pheno = featureTab[,metaFeatures]
    pheno$gender[!pheno$gender %in% c("Male", "Female")] = NA
    pheno$createdon_event = as.Date(pheno$createdon_event)
    pheno$diagnosis.year = as.Date(ISOdate(pheno$diagnosis.year, 1, 1))
    pheno$onset.year = as.Date(ISOdate(pheno$onset.year, 1, 1))
    pheno$timeSinceOnset = as.numeric(pheno$createdon_event - pheno$onset.year)
    features = data.frame(name = rownames(eMatrix), type = "original", 
        description = "original feature", row.names = rownames(eMatrix),
        stringsAsFactors = FALSE)
    logFeatures = grepl("^log_", rownames(eMatrix))
    sigmoidFeatures = grepl("^sigm_", rownames(eMatrix))
    logisticFeatures = grepl("^logist_", rownames(eMatrix))
    features$type[logFeatures] = "log-transformed"
    features$type[sigmoidFeatures] = "sigmoid-transformed"
    features$type[logisticFeatures] = "logistic-transformed"
    features$description[logFeatures] = sprintf("pseudo-log transform of %s",
        sub("^log_", "", rownames(eMatrix)[logFeatures]))
    features$description[sigmoidFeatures] = sprintf("sigmoid transform of %s",
         sub("^sigm_", "", rownames(eMatrix)[sigmoidFeatures]))
    features$description[logisticFeatures] = sprintf("logistic transform of %s",
        sub("^logist_", "", rownames(eMatrix)[logisticFeatures]))
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

load("data/featureTabTraining_Meta.Rda")
load("data/featureTabTest_NoMeta.Rda")
colnames(featureTabTraining) = tolower(make.names(colnames(featureTabTraining)))
colnames(featureTabTest) = tolower(colnames(featureTabTest))
naSums = sapply(featureTabTest, function(x) sum(is.na(x)))
excludeFeatures = names(which(naSums > 1000))
for (f in excludeFeatures) {
    featureTabTraining[[f]] = NULL
}
featureTab = featureTabTraining
rownames(featureTab) <- featureTab$recordid_event
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

featureTab$medtimepoint = sub(".+don't.+", "none", featureTab$medtimepoint)
featureTab$medtimepoint = sub(".+(before|after).+", "\\1", 
    featureTab$medtimepoint)
featureTab$medtimepoint[grepl("^\\W", featureTab$medtimepoint)] = NA

naRecords = is.na(featureTab$medtimepoint) | 
    is.na(featureTab$deep.brain.stimulation) | 
    is.na(featureTab$professional.diagnosis)
featureTab = featureTab[!naRecords,]
exclude = !featureTab$professional.diagnosis != 
    (featureTab$medtimepoint == "none") 
featureTab = featureTab[!exclude,]

featureTab$phoneinf_event = sub("iPod Touch", "iPhone", 
    featureTab$phoneinf_event)
featureTab$phoneinf_event = sub("(iP\\w+\\s*\\d).+$", "\\1", 
    featureTab$phoneinf_event)
featureTab = 
    featureTab[!featureTab$phoneinf_event %in% c("iPhone 4", "iPhone9"),]

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
    logisticName = sprintf("logist_%s", feature)
    transforms[[logisticName]] = logistic(transforms[[feature]])
    kurtosisValues = sapply(transforms, kurtosis, na.rm = TRUE)
    bestTransform = which.min(kurtosisValues)
    bestName = names(transforms)[bestTransform]
    featureTab[[bestName]] = transforms[[bestName]]
}
motionFeatures = colnames(featureTab)[38:ncol(featureTab)] #update motionFeatures

trainingObservations =
    rownames(featureTab[!duplicated(featureTab$healthcode),])
testObservations = rownames(featureTab) %d% trainingObservations
training = table2ExpressionSet(featureTab[trainingObservations,],
    motionFeatures)

testTable = featureTab[testObservations,]
testTable$group = paste(testTable$healthcode, testTable$medtimepoint, sep = ".")
medianFeatures = apply(testTable[,motionFeatures], 2, tapply, testTable$group, 
    median, na.rm = TRUE)
otherVariables = colnames(testTable) %d% motionFeatures
testVarTable = testTable[!duplicated(testTable$group),otherVariables]
rownames(testVarTable) <- testVarTable$group
testTableMedian = cbind(testVarTable, medianFeatures[rownames(testVarTable),])
test = table2ExpressionSet(testTableMedian, motionFeatures)

commonFeatures = intersect(rownames(training), rownames(test))
training = training[commonFeatures,]
test = test[commonFeatures,]

save(training, file="data/training.Rda")
save(test, file="data/test.Rda")
