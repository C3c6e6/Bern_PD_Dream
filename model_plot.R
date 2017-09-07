library(ggplot2)

modelToTable <- function(model, eSet) {
    coefficientVector = coef(model)[-1]
    family = model$family$family
    test = if (family == "binomial") "Chisq" else "F"
    anovaOut = as.data.frame(anova(model, test=test))[-1,]
    features = names(coefficientVector)
    output = fData(eSet)[features,]
    variable = all.vars(as.formula(model$formula))[1]
    if (class(output) == "character") {
        output = data.frame(feature=features, correctionFormula = output,
            row.names = features)
    }
    output$coefficient = as.numeric(coefficientVector)
    colnames(anovaOut)[ncol(anovaOut)] = "p"
    output = cbind(output, anovaOut)
    output = output[order(output$p),]
    output$F = NULL
    output$variable = variable
    output = output[output$Deviance > 1e-4,]
    output
}

scaleValues <- function(values) {
    output = (values - mean(values))/sd(values)
    #output = output/max(output)
    output
}

plotModel <- function(modelTable, eSet) {
    colorVar = unique(modelTable$variable)
    selectedFeatures = as.character(modelTable$feature)
    plotData = pData(eSet)
    if (length(selectedFeatures) == 1) {
        singleFeature = TRUE
        plotData[[selectedFeatures]] = 
            scaleValues(exprs(eSet)[selectedFeatures,])
    } else {
        singleFeature = FALSE
        eMatrix = apply(exprs(eSet)[selectedFeatures,], 1, scaleValues)
        plotData = cbind(plotData, eMatrix)
    }
    molten = melt(plotData, measure.vars = selectedFeatures,
        variable.name = "feature", value.name="value")
    featureTable = fData(eSet)
    if (singleFeature) {
        numericVar = is.numeric(molten[[colorVar]])
        p = ggplot(data = molten, aes_string(x=colorVar, y="value")) +
            geom_point(size = 0.7, 
                position = if (numericVar) "identity" else "jitter") +
            theme(legend.position="bottom") + ggtitle(colorVar) + 
            ylab(selectedFeatures)
    } else {
        p = ggplot(data = molten,
            aes_string(x="feature", y="value", color=colorVar, 
                group="recordId_event")) +
            geom_point(size = 0.7,
                position = if (singleFeature) "jitter" else "identity") +
            geom_line(alpha=0.2, size = 0.3) +
            #geom_violin() +
            theme(legend.position="bottom") + ggtitle(colorVar)
    }
    print(p)
}