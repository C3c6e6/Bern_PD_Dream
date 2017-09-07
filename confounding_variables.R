library(Biobase)
library(ggplot2)

load("data/outbound.Rda")

pheno = pData(outbound)

patientData = pheno[!duplicated(pheno$healthCode),]
patientData = patientData[patientData$gender %in% c("Male", "Female"),]
pdf("results/plots/confounding_variables.pdf")
ggplot(data=patientData, aes(x=professional.diagnosis, y=age, color=gender)) +
     geom_point(position = "jitter", size=0.5, alpha = 0.7) + 
    ggtitle("Age and gender")

table(patientData$gender, patientData$professional.diagnosis)
contingency  = table(patientData$gender, patientData$professional.diagnosis)
fisher.test(contingency[-3,])

load("data/topTables.Rda")
allData = cbind(t(exprs(outbound)), pData(outbound))
ggplot(data=allData, aes(y=acfAJ, x=phoneInf_event)) + 
    geom_point(position="jitter", alpha=0.7) + ggtitle("Phone model") +
    theme(axis.text.x = element_text(angle=90))
ggplot(data=allData, aes(y=logtkeoAA, x=gender)) + 
    geom_point(position="jitter", alpha=0.7) + ggtitle("Gender") +
    theme(axis.text.x = element_text(angle=90))

dev.off()

