# Soundscape Integration

rm(list=ls())

DC = Sys.Date()
library(stringr)
library(lubridate)

# PURPOSE ####
# prep data for automatic labeling
# visualize intergrated data
# save out labeled data

# INPUTS
# (1) by deployment 1-minute TOL files (SanctSound output)
# (2) sound labeling files: vessel detections, AIS transits, species detections
# NOTE: everything set up, but did not label 1-min SPL data because the species detections were on the daily resolution


# SET UP DIRECTORIES ####
dirTop  =  "F:\\RESEARCH\\SanctSound\\" # top directory
loc = "SB"
site    = "SB03" 
dirSPLs    = paste0(dirTop, "data2\\", site, sep="\\")
dirVessels = paste0(dirTop, "data2\\", site, sep="\\")
dirBIO = paste0(dirTop, "data\\", loc,"\\", site)

# INPUT DATA ####
## SOUND LEVEL DATA ####
#FIND FILES
filesSPL = list.files(path = dirSPLs, pattern = "mean_1min", full.names = T)
#READ IN DATA
SPLmin = NULL
for (ii in 1:(length(filesSPL)) )  {
  tmpPSD = read.csv(filesSPL[ii])
  SPLmin = rbind( SPLmin, tmpPSD)
}
rm(tmpPSD)
#FORMAT DATE
SPLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 
## ADD SITE NAME
SPLmin$site = site 

## VESSEL DETECTIONS ####
#FIND FILES
filesVD = list.files(path=dirVessels, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE)
#READ IN DATA
VD = NULL
for (ii in 1:length(filesVD)) {
  tmp = read.csv(filesVD[ii])
  tmp$Site = site
  tmp$Dep  = sapply(strsplit(filesVD[ii], "_"), "[[", 3)
  colnames(tmp) = c("ISOStartTime","ISOEndTime","Label","Site","Dep" )
  VD = rbind(VD,tmp)
}
rm(tmp)
#FORMAT DATA
VD$Start= as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOStartTime)), tz = "GMT" )
VD$End  = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOEndTime)), tz = "GMT" )
VD$Mth  = month(VD$Start )
VD$yr   = year(VD$Start )
VD$Dur  = as.numeric(as.character( difftime(VD$End, VD$Start, units = "secs" )) )
VD$Dep  = as.numeric(as.character(VD$Dep))
VD$DepC = c(0,diff(VD$Dep))
indx = which(VD$DepC == 1) #find transitions in deployments
VD$DurH = VD$Dur/3600 

## AIS DATA ####
#READ IN DATA
AIStran = read.csv(paste0(dirVessels, "smp_transit_data.csv") ) 
#FORMAT DATA
AIStran = AIStran[ AIStran$loc_id == site,]
AIStran$Start = as.POSIXct( gsub("[+]00", "", AIStran$start_time_utc), tz = "GMT" ) 
AIStran$End   = as.POSIXct( gsub("[+]00", "", AIStran$end_time_utc), tz = "GMT" ) 
AIStran$yr = year(AIStran$Start)

## SPECIES DATA ####
#FIND FILES
filesOther= list.files(path=dirBIO, pattern = ".csv", full.names=F, recursive = TRUE)
uBio = gsub(".csv", "", unique( sapply(strsplit(filesOther, "_"), "[[", 5) ) )
idx = !grepl( "BB|OL|TOL|PSD|ships", uBio ) 
uBio = uBio[idx]
#READ IN DATA
filesBIO = NULL
for(bb in 1:length(uBio)) {
  filesBIO = c(filesBIO, list.files(path=dirBIO, pattern = uBio[bb], full.names=T, recursive = TRUE))
}


# LABEL TOL DATA ####