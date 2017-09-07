library(ROCR)
library(Biobase)

getAUC <- function(model, testSet, title) {
    features = all.vars(as.formula(model$formula))[-1]
    testData = cbind(pData(testSet), t(exprs(testSet[features,])))
    modelPrediction = predict(model, newdata = testData)
    pr <- prediction(modelPrediction, testData$professional.diagnosis)
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    auc <- performance(pr, measure = "auc")  
    plot(prf, main = title, 
        sub = sprintf("AUROC = %2.2f%%", auc@y.values[[1]] * 100))
    abline(a=0, b=1, col="red")
    auc@y.values
}

load("data/test_corrected.Rda")

pdf("results/plots/ROCs.pdf")
load("data/glm_spls_professional.diagnosis_model.Rda")
splsAUC = getAUC(model, test, "mixOmics sPLS-DA")
message("sPLS-DA model: ", splsAUC)

load("data/glm_ga_professional.diagnosis_model.Rda")
gaAUC = getAUC(model, test, "glmulti GA")
message("glmulti GA: ", gaAUC)

load("data/glm_exhaustive.Rda")
gaAUC = getAUC(model, test, "glmulti exhaustive")
message("glmulti exhaustive: ", gaAUC)

dev.off()
