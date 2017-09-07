library(synapseClient)

synapseLogin(username = "C3c6e6", 
    apiKey = "b6s1Qd5RoU7OCGSzqGUrTRLM2IHDwgXUDpc2KFd6btcsZprlUU5ySqPRw0TsMS9OggFVVPF8pQQK7fXa+g6YSg==")

demo_syntable <- synTableQuery("SELECT * FROM syn10146552")
demo <- demo_syntable@values
healthCodeCol <- c(as.character(demo$healthCode))
healthCodeList <- paste0(sprintf("'%s'", healthCodeCol), collapse = ", ")

INPUT_WALKING_ACTIVITY_TABLE_SYNID = 'syn10146553'
actv_walking_syntable <- synTableQuery(paste0("SELECT 'recordId', 'healthCode','deviceMotion_walking_outbound.json.items' FROM ", INPUT_WALKING_ACTIVITY_TABLE_SYNID, " WHERE healthCode IN ", "(", healthCodeList, ") LIMIT 10"))
actv_walking <- actv_walking_syntable@values
actv_walking$idx <- rownames(actv_walking)

outbound_Walking_json_files <- synDownloadTableColumns(actv_walking_syntable, "deviceMotion_walking_outbound.json.items")
outbound_Walking_json_files <- data.frame(outbound_Walking_json_fileId =names(outbound_Walking_json_files),
    outbound_Walking_json_file = as.character(outbound_Walking_json_files))

outbound_Walking_json_files <- outbound_Walking_json_files %>%
    distinct(outbound_Walking_json_file, .keep_all = TRUE)

