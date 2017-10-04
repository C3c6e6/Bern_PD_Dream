library(mice) 
source("functions.R")

load("data/featureTabALL_NoMeta.Rda")
load("data/glm_stepped.Rda")

selectedFeatures = all.vars(as.formula(model$formula))[-1]

squaredRegex = "_2$"
transformationRegex = "^(log|logist|sigm)_(.+)"
transformations = sub(transformationRegex, "\\1", selectedFeatures)
transformations[!grepl(transformationRegex, selectedFeatures)] = NA
squaredFeatures = grepl(squaredRegex, selectedFeatures)
originalFeatures = sub(squaredRegex, "",
    sub(transformationRegex, "\\2", selectedFeatures))
originalFeatures = tolower(originalFeatures)
colnames(featureTab) = tolower(colnames(featureTab))
featureTable = featureTab[,originalFeatures]

names(transformations) <- originalFeatures
transformationFunctions = list(logist = logistic, log = logTransform, 
    sigm = sigmoid)
for (name in names(transformations)[!is.na(transformations)]) {
    transformationType = transformations[name]
    message(name, ": ", transformationType)
    functionToUse = transformationFunctions[[transformationType]]
    featureTable[[name]] = functionToUse(featureTable[[name]])
}
for (name in originalFeatures[squaredFeatures]) {
    featureTable[[name]] = featureTable[[name]] ^ 2
}
sapply(featureTable, function(x) sum(is.na(x)))
featureTable = complete(mice(featureTable))
sapply(featureTable, function(x) sum(is.na(x)))
featureTable$recordId <- featureTab$recordid

featureTable = featureTable[,rev(colnames(featureTable))]
write.table(featureTable, file="results/submissions.csv", sep = ",",
    row.names = FALSE, col.names = TRUE, quote = FALSE)
