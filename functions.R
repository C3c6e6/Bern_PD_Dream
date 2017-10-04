aDataFrame <- function(input) {
    metaData = data.frame(labelDescription=colnames(input), 
        row.names=colnames(input))
    new("AnnotatedDataFrame", data=input, varMetadata=metaData)
}

addFloor <- function(input, b) {
    floorValue = floor(min(log(input[input > 0], base = b), na.rm = TRUE))
    floorValue = min(c(floorValue, 0))
    input[input <= 0] = 2^floorValue
    input
}

corDist = function(x) 
    as.dist(1-cor(t(x), use = "pairwise.complete.obs", method="spearman"))

getNullModel <- function(variable, dataset) {
    family = if(is.numeric(dataset[[variable]])) "gaussian" else "binomial"
    glm(sprintf("%s ~ 1", variable), family=family, data=dataset)    
}

getModelPValue <- function(model) {
    if (length(coef(model)) == 1) {
        return(1)
    }
    variable = all.vars(as.formula(model$formula))[1]
    features = names(coef(model)[-1])
    dataset = model$data
    exclude = is.na(dataset[[variable]])
    dataset = dataset[!exclude,]
    nullModel = getNullModel(variable, dataset)
    anova(model, nullModel, test="Chisq")$`Pr(>Chi)`[2]
} 

isOutlier <- function(x, k) {
    five = fivenum(x)
    lowerHinge = five[2]
    upperHinge = five[4]
    iqr = upperHinge - lowerHinge
    minValue = lowerHinge - k * iqr
    maxValue = upperHinge + k * iqr
    return (x < minValue | x > maxValue)
}

logTransform <- function(x, b = 2) {
    negativeValues = x < 0
    positiveValues = x >= 0
    if (any(is.na(x))) {
        negativeValues[is.na(x)] = FALSE
        positiveValues[is.na(x)] = FALSE
    }
    y = x
    if (any(negativeValues)) {
        y[negativeValues] = -log(-x[negativeValues], base = b)
    }
    if (any(positiveValues)) {
        y[positiveValues] = log(addFloor(x[positiveValues], b), base = b)
        if (any(negativeValues)) {
            offset = max(y[negativeValues]) - min(y[positiveValues])
            y[positiveValues] = y[positiveValues] + offset
        }
    }
    y
}

sigmoid <- function(x) {
    x/(1+abs(x))
}

logistic = function(x) {
    1/(1+exp(-x))
}
