library(Biobase)

args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
load(inputFile)

eMatrix = exprs(training)
pheno = pData(training)
modelData = as.data.frame(t(eMatrix))
modelData$professional.diagnosis = pheno$professional.diagnosis

formulaString = sprintf("professional.diagnosis ~ 1 + %s",
    paste(rownames(eMatrix), collapse = " + "))
model = glm(as.formula(formulaString), data = modelData)
save(model, file = "data/glm_full.Rda")

model = step(model, direction = "both")
save(model, file = "data/glm_stepped.Rda")