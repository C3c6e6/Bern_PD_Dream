
# Collect table of feature per recordID

# We assume that
# ... the demographic data demo_syntable@values is saved in demo.Rda
# ... the file table activ_wholetab@values
#     (synTableQuery("SELECT * FROM syn10146553")) in filetab.Rda


# load all necessary libraries
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

# + Achu:
library(caTools)
library(signal)
library(igraph)
library(zoo)

# Moved walking feature extraction to local file walkingModule_mod.R,
# for convenience, so we don't need mpowertools:
#library(mpowertools) # to install mpowertools run: library(devtools);
#                       devtools::install_github("Sage-Bionetworks/mpowertools")

source("walkingModule_mod.R")


#Load demo and filetab:

load("demo.Rda")
load("filetab.Rda")


# Make one big table with whole of the info:

mytab=merge(filetab,demo,all.x = TRUE,by="healthCode")

#Change names for clarity:

names(mytab)[names(mytab) == 'recordId.x'] <- 
  'recordId_event'
names(mytab)[names(mytab) == 'recordId.y'] <- 
  'recordId_demo'
names(mytab)[names(mytab) == 'createdOn.x'] <- 
  'createdOn_event'
names(mytab)[names(mytab) == 'createdOn.y'] <- 
  'createdOn_demo'
names(mytab)[names(mytab) == 'appVersion.x'] <- 
  'appVersion_event'
names(mytab)[names(mytab) == 'appVersion.y'] <- 
  'appVersion_demo'
names(mytab)[names(mytab) == 'phoneInfo.x'] <- 
  'phoneInf_event'
names(mytab)[names(mytab) == 'phoneInfo.y'] <- 
  'phoneInfo_demo'


# My data is on two separate location: change to suit
# and I need to swap between the two on occasion:
#mytab[] <- lapply(mytab, gsub, 
#                pattern = "/media/achu/Data", 
#                replacement = "/media/annechristine/Data1", fixed = TRUE)


 if (detectCores() >= 2) {
   runParallel <- TRUE
 } else {
   runParallel <- FALSE
 }
 

#registerDoMC(detectCores() - 2)
registerDoMC(detectCores())



##### RAW Accel OUTBOUND  ####
# Raw acceleration, outbound

# Files are in col:
nColFile=6

#Pick rows for which the file exists and is not empty
seltab=mytab[which(!(is.na(mytab[,c(nColFile)]))),]
seltab=seltab[which((file.size(seltab[,c(nColFile)])>4) ),]


# ddply does not like the whole matrix (boolean cols seems to trouble it:
# we will merge the rest later then

myrange=c(1:2,15,nColFile) 
selcolname=colnames(seltab)[nColFile]

outbound_RawAccel <-
  ddply(
    .data = seltab[,myrange], .variables = colnames(seltab[,myrange]),
    .fun = function(row) {
      getWalkFeaturesAccel(row[,c(selcolname)],row[,c("idx")],"Accel_Outbnd")
    },
    .parallel = TRUE
  )


save(outbound_RawAccel,file="outbound_Accel.Rda")




#### Rotrate OUTBOUND  ####
# Rotational Rate in DevMotion, outbound

# Files are in col:
nColFile=7

#Pick rows for which the file exists and is not empty
seltab=mytab[which(!(is.na(mytab[,c(nColFile)]))),]
seltab=seltab[which((file.size(seltab[,c(nColFile)])>4) ),]


# ddply does not like the whole matrix (boolean cols seems to trouble it:
# we will merge the rest later then
myrange=c(1:2,15,nColFile) #
selcolname=colnames(seltab)[nColFile]

outbound_RotRate <-
  ddply(
    .data = seltab[,myrange], .variables = colnames(seltab[,myrange]),
    .fun = function(row) {
      getWalkFeaturesRotRate(row[,c(selcolname)],row[,c("idx")],"RotRate_Outbnd")
    },
    .parallel = TRUE
  )


save(outbound_RotRate,file="outbound_RotRate.Rda")




##### RAW Accel REST ####
# Raw acceleration, Rest

# files are in col:
nColFile=12

#Pick rows for which the file exists and is not empty
seltab=mytab[which(!(is.na(mytab[,c(nColFile)]))),]
seltab=seltab[which((file.size(seltab[,c(nColFile)])>4) ),] 


# ddply does not like the whole matrix (boolean cols seems to trouble it: 
# we will merge the rest later then
myrange=c(1:2,15,nColFile) #
selcolname=colnames(seltab)[nColFile]

rest_RawAccel <-
  ddply(
    .data = seltab[,myrange], .variables = colnames(seltab[,myrange]),
    .fun = function(row) {
      getWalkFeaturesAccel(row[,c(selcolname)],row[,c("idx")],"Accel_Rest")
    },
    .parallel = TRUE
  )


save(rest_RawAccel,file="rest_Accel.Rda")


 ##### Roterate REST ####
 # Rotational Rate in DevMotion, Rest
 # CAUTION: three files were corrupted
 # (at collection, from download, copy, transert...?)
 # There were 9307, 33637  and 33680 in my seltab above
 # They were removed by hand as a temporary fix
 
 # Outbound RotRate (in devMot files)
 sensType="RotRate"
 
 # Files are in col:
 nColFile=13
 
 #Pick rows for which the file exists and is not empty
 seltab=mytab[which(!(is.na(mytab[,c(nColFile)]))),]
 seltab=seltab[which((file.size(seltab[,c(nColFile)])>4) ),] 
 
 
 # ddply does not like the whole matrix (boolean cols seems to trouble it: 
 # we will merge the rest later then
 myrange=c(1:2,15,nColFile) #
 selcolname=colnames(seltab)[nColFile]
 
 rest_RotRate <-
   ddply(
     .data = seltab[,myrange], .variables = colnames(seltab[,myrange]),
     .fun = function(row) {
       getWalkFeaturesRotRate(row[,c(selcolname)],row[,c("idx")],"RotRate_rest")
     },
     .parallel = TRUE
   )
 
 
 save(rest_RotRate,file="rest_RotRate.Rda")



##### RAW Accel RETURN  ####
# Raw acceleration, return

# files are in col:
nColFile=9

#Pick rows for which the file exists and is not empty
seltab=mytab[which(!(is.na(mytab[,c(nColFile)]))),]
seltab=seltab[which((file.size(seltab[,c(nColFile)])>4) ),]


# ddply does not like the whole matrix (boolean cols seems to trouble it:
# we will merge the rest later then
myrange=c(1:2,15,nColFile) #
selcolname=colnames(seltab)[nColFile]

return_RawAccel <-
  ddply(
    .data = seltab[,myrange], .variables = colnames(seltab[,myrange]),
    .fun = function(row) {
      getWalkFeaturesAccel(row[,c(selcolname)],row[,c("idx")],"Accel_Ret")
    },
    .parallel = TRUE
  )


save(return_RawAccel,file="return_Accel.Rda")



#### Rotrate RETURN  ####
# Rotational Rate in DevMotion, Return

# Return RotRate (in devMot files)
sensType="RotRate"

# Files are in col:
nColFile=10

#Pick rows for which the file exists and is not empty
seltab=mytab[which(!(is.na(mytab[,c(nColFile)]))),]
seltab=seltab[which((file.size(seltab[,c(nColFile)])>4) ),] 


# ddply does not like the whole matrix (boolean cols seems to trouble it: 
# we will merge the rest later then
myrange=c(1:2,15,nColFile) #
selcolname=colnames(seltab)[nColFile]

return_RotRate <-
  ddply(
    .data = seltab[,myrange], .variables = colnames(seltab[,myrange]),
    .fun = function(row) {
      getWalkFeaturesRotRate(row[,c(selcolname)],row[,c("idx")],"RotRate_Ret")
    },
    .parallel = TRUE
  )


save(return_RotRate,file="return_RotRate.Rda")



### PEDOMETER outbound #####
nColFile=8

#Pick rows for which the file exists and is not empty
seltab=mytab[which(!(is.na(mytab[,c(nColFile)]))),]
seltab=seltab[which((file.size(seltab[,c(nColFile)])>4) ),] 


# ddply does not like the whole matrix (boolean cols seems to trouble it: 
# we will merge the rest later then
myrange=c(1:2,15,nColFile) #
selcolname=colnames(seltab)[nColFile]
 
 pedomOutboundFeatures <-
   ddply(
     .data = seltab[,myrange], .variables = colnames(seltab[,myrange]),
     .fun = function(row) {
       getPedometerFeatures(row[,c(selcolname)],"outbound")
     },
     .parallel = TRUE
   )
 
 save(pedomOutboundFeatures,file="outbound_pedom.Rda")

 ### PEDOMETER return #####
 nColFile=11
   
#Pick rows for which the file exists and is not empty
seltab=mytab[which(!(is.na(mytab[,c(nColFile)]))),]
seltab=seltab[which((file.size(seltab[,c(nColFile)])>4) ),] 
 
 
 # ddply does not like the whole matrix (boolean cols seems to trouble it: 
 # we will merge the rest later then
 myrange=c(1:2,15,nColFile) #
 selcolname=colnames(seltab)[nColFile]
 
 pedomReturnFeatures <-
   ddply(
     .data = seltab[,myrange], .variables = colnames(seltab[,myrange]),
     .fun = function(row) {
       getPedometerFeatures(row[,c(selcolname)],"return")
     },
     .parallel = TRUE
   )
 
save(pedomReturnFeatures,file="return_pedom.Rda")




# We need to collate the rest of the info to the calcultations:

mytabINT=mytab[,c(1:5,14:45)] #table of interest

# Merge calculation (minus the info about the file name)

featureTab<-merge(mytabINT,outbound_RawAccel[,-c(4)],all.x=TRUE)
featureTab<-merge(featureTab,rest_RawAccel[,-c(4)],all.x=TRUE)
featureTab<-merge(featureTab,return_RawAccel[,-c(4)],all.x=TRUE)
featureTab<-merge(featureTab,outbound_RotRate[,-c(4)],all.x=TRUE)
featureTab<-merge(featureTab,rest_RotRate[,-c(4)],all.x=TRUE)
featureTab<-merge(featureTab,return_RotRate[,-c(4)],all.x=TRUE)

featureTab<-merge(featureTab,pedomOutboundFeatures[,-c(4)],all.x=TRUE)
featureTab<-merge(featureTab,pedomReturnFeatures[,-c(4)],all.x=TRUE)

save(featureTab,file="features_clipExtra_v8.Rda")

### Removed return_error and outbound_error (pedometer data error reports)
featureTab<-featureTab[-c(1010,1017)]

### Incidentally, did the same for feature matrix of earlier data, and collated them together

### Change the momentInDayFormat.json.choiceAnswers to medTimepoint for consistancy for earlier training:


names(featureTab)[names(featureTab) == 'momentInDayFormat.json.choiceAnswers'] <- 'medTimepoint'
featureTab_AllTrain<-rbind(featureTabTrain,featureTab)
save(featureTab,file="features_AllTrain.Rda")
