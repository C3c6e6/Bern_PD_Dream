# Collect acceleration data

# load all necessary libraries
library(synapseClient)
synapseLogin()
library(plyr)
library(dplyr)
library(ggplot2)
library(doMC)
library(jsonlite)
library(parallel)
library(tidyr)
library(lubridate)
library(stringr)
library(sqldf)
library(plyr)
library(e1071)
library(mpowertools) # to install mpowertools run: library(devtools); devtools::install_github("Sage-Bionetworks/mpowertools")

synapseCacheDir('/media/annechristine/Data1/ParkinsonDataExtra')

# read in the healthCodes of interest from demographics training table
demo_syntable <- synTableQuery("SELECT * FROM syn10146552")
demo <- demo_syntable@values

#Other choices: demo_syntable@updateEtag, demo_syntable@schema
healthCodeCol <- c(as.character(demo$healthCode))
healthCodeList <- paste0(sprintf("'%s'", healthCodeCol), collapse = ", ")


INPUT_WALKING_ACTIVITY_TABLE_SYNID = 'syn10733835'

####### DEVICEMOTION outbound  #########

# Query table of interest, walking training table

actv_walking_syntable <- synTableQuery(paste0("SELECT 'recordId', 
                                              'healthCode',
   'deviceMotion_walking_outbound.json.items' FROM ", 
    INPUT_WALKING_ACTIVITY_TABLE_SYNID, " WHERE healthCode IN ", "(", healthCodeList, ")"))

actv_walking <- actv_walking_syntable@values
actv_walking$idx <- rownames(actv_walking)

#download outbound walking json files
deviceMotion_walking_outbound_json_files <- synDownloadTableColumns(actv_walking_syntable, "deviceMotion_walking_outbound.json.items")

deviceMotion_walking_outbound_json_files <- data.frame(deviceMotion_outbound_json_fileId =
                                                         names(deviceMotion_walking_outbound_json_files),
                                                         deviceMotion_walking_outbound_json_files = 
                                              as.character(deviceMotion_walking_outbound_json_files))

deviceMotion_walking_outbound_json_files <- deviceMotion_walking_outbound_json_files %>%
  distinct(deviceMotion_walking_outbound_json_files, .keep_all = TRUE)

actv_walking <- merge(actv_walking,deviceMotion_walking_outbound_json_files, by.x="deviceMotion_walking_outbound.json.items", 
                       by.y="deviceMotion_outbound_json_fileId", all=TRUE)



# add walking json files columns
actv_walking <- actv_walking %>% mutate(deviceMotion_walking_outbound_json_files = as.character(deviceMotion_walking_outbound_json_files))

# remove duplicates
actv_walking <- actv_walking %>%
  distinct(deviceMotion_walking_outbound_json_files, .keep_all = TRUE)

actv_walking_DEVMOT_OUTBOUND<-actv_walking



####### DEVICEMOTION return  #########

# Query table of interest, walking training table

actv_walking_syntable <- synTableQuery(paste0("SELECT 'recordId', 
                                              'healthCode',
                                              'deviceMotion_walking_return.json.items' FROM ", 
                                              INPUT_WALKING_ACTIVITY_TABLE_SYNID, " WHERE healthCode IN ", "(", healthCodeList, ")"))

actv_walking <- actv_walking_syntable@values
actv_walking$idx <- rownames(actv_walking)


#download return walking json files
deviceMotion_walking_return_json_files <- synDownloadTableColumns(actv_walking_syntable, 
                    "deviceMotion_walking_return.json.items")


deviceMotion_walking_return_json_files <- data.frame(deviceMotion_return_json_fileId=
                                          names(deviceMotion_walking_return_json_files),
                                          deviceMotion_walking_return_json_files= 
                                          as.character(deviceMotion_walking_return_json_files))

deviceMotion_walking_return_json_files <- deviceMotion_walking_return_json_files %>%
  distinct(deviceMotion_walking_return_json_files, .keep_all = TRUE)

actv_walking <- merge(actv_walking,deviceMotion_walking_return_json_files,
                      by.x="deviceMotion_walking_return.json.items", 
                      by.y="deviceMotion_return_json_fileId", all=TRUE)

# add walking json files columns
actv_walking <- actv_walking %>% mutate(deviceMotion_walking_return_json_files = 
                                          as.character(deviceMotion_walking_return_json_files))

# remove duplicates
actv_walking <- actv_walking %>%
  distinct(deviceMotion_walking_return_json_files, .keep_all = TRUE)


actv_walking_DEVMOT_RETURN<-actv_walking

####### DEVICEMOTION rest  ######
# Query 'walking training table' for rest data

actv_walking_syntable <- synTableQuery(paste0("SELECT 'recordId', 
                                              'healthCode',
                                              'deviceMotion_walking_rest.json.items' FROM ", 
                                              INPUT_WALKING_ACTIVITY_TABLE_SYNID, " WHERE healthCode IN ", "(", healthCodeList, ")"))


actv_walking <- actv_walking_syntable@values
actv_walking$idx <- rownames(actv_walking)


#download rest walking json files
deviceMotion_walking_rest_json_files <- synDownloadTableColumns(actv_walking_syntable, 
                                                                  "deviceMotion_walking_rest.json.items")


deviceMotion_walking_rest_json_files <- data.frame(deviceMotion_rest_json_fileId=
                                                       names(deviceMotion_walking_rest_json_files),
                                                     deviceMotion_walking_rest_json_files= 
                                                       as.character(deviceMotion_walking_rest_json_files))

deviceMotion_walking_rest_json_files <- deviceMotion_walking_rest_json_files %>%
  distinct(deviceMotion_walking_rest_json_files, .keep_all = TRUE)

actv_walking <- merge(actv_walking,deviceMotion_walking_rest_json_files,
                      by.x="deviceMotion_walking_rest.json.items", 
                      by.y="deviceMotion_rest_json_fileId", all=TRUE)

# add walking json files columns
actv_walking <- actv_walking %>% mutate(deviceMotion_walking_rest_json_files = 
                                          as.character(deviceMotion_walking_rest_json_files))

# remove duplicates
actv_walking <- actv_walking %>%
  distinct(deviceMotion_walking_rest_json_files, .keep_all = TRUE)


actv_walking_DEVMOT_REST<-actv_walking





####### ACCEL walking outbound  #########

# Query table of interest, walking training table

actv_walking_syntable <- synTableQuery(paste0("SELECT 'recordId', 
                                              'healthCode',
                                              'accel_walking_outbound.json.items' FROM ", 
                                              INPUT_WALKING_ACTIVITY_TABLE_SYNID, " WHERE healthCode IN ", "(", healthCodeList, ")"))




actv_walking <- actv_walking_syntable@values
actv_walking$idx <- rownames(actv_walking)

#download outbound walking json files
accel_walking_outbound_json_files <- synDownloadTableColumns(actv_walking_syntable, "accel_walking_outbound.json.items")

accel_walking_outbound_json_files <- data.frame(accel_outbound_json_fileId =
                                                         names(accel_walking_outbound_json_files),
                                                       accel_walking_outbound_json_files = 
                                                         as.character(accel_walking_outbound_json_files))

accel_walking_outbound_json_files <- accel_walking_outbound_json_files %>%
  distinct(accel_walking_outbound_json_files, .keep_all = TRUE)

actv_walking <- merge(actv_walking,accel_walking_outbound_json_files, by.x="accel_walking_outbound.json.items", 
                      by.y="accel_outbound_json_fileId", all=TRUE)



# add walking json files columns
actv_walking <- actv_walking %>% mutate(accel_walking_outbound_json_files = as.character(accel_walking_outbound_json_files))

# remove duplicates
actv_walking <- actv_walking %>%
  distinct(accel_walking_outbound_json_files, .keep_all = TRUE)

actv_walking_ACCEL_OUTBOUND<-actv_walking



####### ACCEL return #########


# Query table of interest, walking training table

actv_walking_syntable <- synTableQuery(paste0("SELECT 'recordId', 
                                              'healthCode',
                                              'accel_walking_return.json.items' FROM ", 
                                              INPUT_WALKING_ACTIVITY_TABLE_SYNID, " WHERE healthCode IN ", "(", healthCodeList, ")"))


actv_walking <- actv_walking_syntable@values
actv_walking$idx <- rownames(actv_walking)



#download return walking json files

accel_walking_return_json_files <- synDownloadTableColumns(actv_walking_syntable, 
                                                                  "accel_walking_return.json.items")


accel_walking_return_json_files <- data.frame(accel_return_json_fileId=
                                                       names(accel_walking_return_json_files),
                                                     accel_walking_return_json_files= 
                                                       as.character(accel_walking_return_json_files))

accel_walking_return_json_files <- accel_walking_return_json_files %>%
  distinct(accel_walking_return_json_files, .keep_all = TRUE)

actv_walking <- merge(actv_walking,accel_walking_return_json_files,
                      by.x="accel_walking_return.json.items", 
                      by.y="accel_return_json_fileId", all=TRUE)

# add walking json files columns
actv_walking <- actv_walking %>% mutate(accel_walking_return_json_files = 
                                          as.character(accel_walking_return_json_files))

# remove duplicates
actv_walking <- actv_walking %>%
  distinct(accel_walking_return_json_files, .keep_all = TRUE)


actv_walking_ACCEL_RETURN<-actv_walking



####### ACCEL walking rest #########


# Query 'walking training table' for rest data

actv_walking_syntable <- synTableQuery(paste0("SELECT 'recordId', 
                                              'healthCode',
                                              'accel_walking_rest.json.items' FROM ", 
                                              INPUT_WALKING_ACTIVITY_TABLE_SYNID, " WHERE healthCode IN ", "(", healthCodeList, ")"))


actv_walking <- actv_walking_syntable@values
actv_walking$idx <- rownames(actv_walking)


#download rest walking json files
accel_walking_rest_json_files <- synDownloadTableColumns(actv_walking_syntable, 
                                                                "accel_walking_rest.json.items")


accel_walking_rest_json_files <- data.frame(accel_rest_json_fileId=
                                                     names(accel_walking_rest_json_files),
                                                   accel_walking_rest_json_files= 
                                                     as.character(accel_walking_rest_json_files))

accel_walking_rest_json_files <- accel_walking_rest_json_files %>%
  distinct(accel_walking_rest_json_files, .keep_all = TRUE)

actv_walking <- merge(actv_walking,accel_walking_rest_json_files,
                      by.x="accel_walking_rest.json.items", 
                      by.y="accel_rest_json_fileId", all=TRUE)

# add walking json files columns
actv_walking <- actv_walking %>% mutate(accel_walking_rest_json_files = 
                                          as.character(accel_walking_rest_json_files))

# remove duplicates
actv_walking <- actv_walking %>%
  distinct(accel_walking_rest_json_files, .keep_all = TRUE)


actv_walking_ACCEL_REST<-actv_walking




####### PEDOMETER outbound  #########
# Query table of interest, walking training table
actv_walking_syntable <- synTableQuery(paste0("SELECT 'recordId', 
                                              'healthCode',
                                              'pedometer_walking_outbound.json.items' FROM ", 
                                              INPUT_WALKING_ACTIVITY_TABLE_SYNID, " WHERE healthCode IN ", "(", healthCodeList, ")"))


actv_walking <- actv_walking_syntable@values
actv_walking$idx <- rownames(actv_walking)


pedometer_walking_outbound_json_files <- synDownloadTableColumns(actv_walking_syntable, 
                        "pedometer_walking_outbound.json.items")


pedometer_walking_outbound_json_files <- data.frame(pedometer_outbound_json_fileId =
                                                         names(pedometer_walking_outbound_json_files),
                                                       pedometer_walking_outbound_json_files = 
                                                         as.character(pedometer_walking_outbound_json_files))

pedometer_walking_outbound_json_files <- pedometer_walking_outbound_json_files %>%
  distinct(pedometer_walking_outbound_json_files, .keep_all = TRUE)

actv_walking <- merge(actv_walking,pedometer_walking_outbound_json_files, by.x="pedometer_walking_outbound.json.items", 
                      by.y="pedometer_outbound_json_fileId", all=TRUE)



# add walking json files columns
actv_walking <- actv_walking %>% mutate(pedometer_walking_outbound_json_files = as.character(pedometer_walking_outbound_json_files))

# remove duplicates
actv_walking <- actv_walking %>%
  distinct(pedometer_walking_outbound_json_files, .keep_all = TRUE)

actv_walking_PEDOMETER_OUTBOUND<-actv_walking




####### PEDOMETER return  #########

# Query table of interest, walking training table
actv_walking_syntable <- synTableQuery(paste0("SELECT 'recordId', 
                                              'healthCode',
                                              'pedometer_walking_return.json.items' FROM ", 
                                              INPUT_WALKING_ACTIVITY_TABLE_SYNID, " WHERE healthCode IN ", "(", healthCodeList, ")"))



actv_walking <- actv_walking_syntable@values
actv_walking$idx <- rownames(actv_walking)

pedometer_walking_return_json_files <- synDownloadTableColumns(actv_walking_syntable, 
                                                                 "pedometer_walking_return.json.items")


pedometer_walking_return_json_files <- data.frame(pedometer_return_json_fileId =
                                                      names(pedometer_walking_return_json_files),
                                                    pedometer_walking_return_json_files = 
                                                      as.character(pedometer_walking_return_json_files))

pedometer_walking_return_json_files <- pedometer_walking_return_json_files %>%
  distinct(pedometer_walking_return_json_files, .keep_all = TRUE)

actv_walking <- merge(actv_walking,pedometer_walking_return_json_files, by.x="pedometer_walking_return.json.items", 
                      by.y="pedometer_return_json_fileId", all=TRUE)



# add walking json files columns
actv_walking <- actv_walking %>% mutate(pedometer_walking_return_json_files = as.character(pedometer_walking_return_json_files))

# remove duplicates
actv_walking <- actv_walking %>%
  distinct(pedometer_walking_return_json_files, .keep_all = TRUE)

actv_walking_PEDOMETER_RETURN<-actv_walking


######### Putting things together ####
wholetab <- synTableQuery(paste0("SELECT * FROM ", 
                                 INPUT_WALKING_ACTIVITY_TABLE_SYNID, " WHERE healthCode IN ", "(", healthCodeList, ")"))
wholetab<-wholetab@values
wholetab$idx <- rownames(wholetab)


filetab<-merge(wholetab,actv_walking_ACCEL_OUTBOUND,all.x = TRUE,by=c("idx","recordId","healthCode",
                                                                      "accel_walking_outbound.json.items"))


filetab<-merge(filetab,actv_walking_DEVMOT_OUTBOUND,all.x = TRUE,by=c("idx","recordId","healthCode",
                                                                      "deviceMotion_walking_outbound.json.items"))


filetab<-merge(filetab,actv_walking_PEDOMETER_OUTBOUND,all.x = TRUE,by=c("idx","recordId","healthCode",
                                                                      "pedometer_walking_outbound.json.items"))


filetab<-merge(filetab,actv_walking_ACCEL_RETURN,all.x = TRUE,by=c("idx","recordId","healthCode",
                                                                   "accel_walking_return.json.items"))


filetab<-merge(filetab,actv_walking_DEVMOT_RETURN,all.x = TRUE,by=c("idx","recordId","healthCode",
                                                                    "deviceMotion_walking_return.json.items"))


filetab<-merge(filetab,actv_walking_PEDOMETER_RETURN,all.x = TRUE,by=c("idx","recordId","healthCode",
                                                                         "pedometer_walking_return.json.items"))


filetab<-merge(filetab,actv_walking_ACCEL_REST,all.x = TRUE,by=c("idx","recordId","healthCode",
                                                                      "accel_walking_rest.json.items"))


filetab<-merge(filetab,actv_walking_DEVMOT_REST,all.x = TRUE,by=c("idx","recordId","healthCode",
                                                                      "deviceMotion_walking_rest.json.items"))

# Eliminate *items*, not necessary
filetab<-filetab[-c(4:11)]

# for consistency with earlier training test, move things around
filetab<-filetab[c(2:6,8:15,7,1)]
save(filetab,file="filetab.Rda")
save(demo,file="demo.Rda")
