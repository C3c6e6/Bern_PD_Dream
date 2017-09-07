
args = commandArgs(trailingOnly = TRUE)
inputFile = args[1]
outputDir = args[2]
load(inputFile)

for (name in names(topTables)) {
    topTable = topTables[[name]]
    outputFile = sprintf("%s/%s.txt", outputDir, name)
    write.table(topTable, outputFile, col.names = NA, row.names = TRUE,
        sep="\t", quote=FALSE)
}
