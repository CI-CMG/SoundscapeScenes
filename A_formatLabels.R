# Format detection data for HMD combine 
#each detection type will be different, so only runs one site at a time

# INPUT
#directory of detection data (any format)
#seasonal data (that matches )

# OUTPUT
#all detections combined with standard headings (csv format) with
#Headings = ("sourceType","totalDets","hours","seasons")

# NEXT
#ADD wind data inputs and categories (e.g.SoundCoop portal)
#ADD vessel detections from Triton
#ADD googleAI humpback detections
#ADD other species detection types with start/end times (e.g. fish chorus)
#GUI to read in and format detection and environmental files

rm(list=ls()) 

# LIBRARIES ####
library(tidyverse)
library(readxl)
library(dplyr)
library(ncdf4)

# PARAMS ####
siteN = "AU_CH01"
#siteN = "SB03"
gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
dirOut =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS" )
DC = Sys.Date()

# AU_CHO1 ####
if (siteN = "AU_CH01") {
  ## DETECTIONS FILES ####
  siteD = paste0(siteN,"\\IP16_AU_CH01_Excel files")
  inDir = paste0( gdrive, "SoundCoop_AcousticScene\\DATA\\detections\\", siteD )
  inFiles  = list.files( inDir, pattern = "Any", full.names = T, recursive = T)     
  inFiles2 = list.files( inDir, pattern = "Nothing", full.names = T, recursive = T) 
  inFiles = c(inFiles, inFiles2)
  
  # FORMAT DETECTIONS ####
  detSum = NULL 
  detAll = NULL
  for (f in 1:length(inFiles)) {  # f = 1
    
    inTmp = read_excel(inFiles[f])  
    inTmp = inTmp[,1:4]
    colnames(inTmp) = c("startTime","endTime","mins","season")
    inTmp = inTmp[!is.na(inTmp$startTime),]
    sourceType =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 1) #sourceType
    colnames(inTmp) = c("Start","End","mins","season")
    inTmp$sourceType = sourceType
    
    #summary of each detections types- total detections and hours with signal
    detSum = rbind(detSum, c(sourceType, nrow(inTmp), sum(inTmp$mins)/60, length( unique(inTmp$season)  ) ) )
    
    # combine into one file
    detAll = rbind(detAll, inTmp )
  }
  detSum = as.data.frame(detSum) 
  colnames(detSum) = c("sourceType","totalDets","hours","seasons")
  #detAll$Minutes = difftime( detAll$End,detAll$Start,units = "mins")
  detAll$Day = as.Date(detAll$Start)
  detAll$Dayed = as.Date(detAll$End)
  unique(detAll$sourceType)
  colnames(detAll)
  
  # CHECKS ####
  # Are all detections start and end on the same day?
  # which( (detAll$Day - detAll$Dayed )>0 ) #CH01 yes-  not likely the case for all 
  
}

if (siteN  = "SB03") {
  
  inSites = c( "SB03_01","SB03_02","SB03_03","SB03_04","SB03_05","SB03_06","SB03_07","SB03_08",
               "SB03_09","SB03_10","SB03_11","SB03_12","SB03_13","SB03_14",
               "SB03_15", "SB03_16", "SB03_17", "SB03_18", "SB03_19", "SB03_20")
  
  ## DETECTIONS FILES ####
  dirTop = "F:\\SanctSound" 
  dirDets = paste0(dirTop, "//", inSites, "\\detections\\")
  detFiles = list.files(path = dirDets, full.names = T, recursive = T)
  detFiles = detFiles[!grepl("1d", detFiles)] #remove 1 day files
  detFiles = detFiles[!grepl("1h", detFiles)] #remove 1 hr files
  detFiles = detFiles[!grepl("\\.nc", detFiles)] #remove nc files
  detFiles = detFiles[!grepl("metadata", detFiles)] #remove metadata
  # basename(detFiles)
  detTypes = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(detFiles)), "_"), "[[", 4) #site name
  siteD = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(detFiles)), "_"), "[[", 3)
  
  # FORMAT DETECTIONS ####
  detAll = NULL
  for (dd in 1:length(detFiles) ) { # dd = 1
    
    inTmp = tolower( detTypes[dd] )
    inFile = detFiles [dd]
    siteOut = paste0( siteN, "_", siteD[dd])
    
    ## VESSEL detections ####
    if (inTmp == "ships" ){
      tmp = read.csv(inFile)
      colnames(tmp) = c("ISOStartTime","ISOEndTime","Label" )
      if (tmp$Label[1] != "NoShip") {
        
        tmp$Site = siteOut
        tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
        tmp$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOEndTime)),   tz = "GMT" )
       
        tmp$mins = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "mins" )) )
        tmp$sourceType = inTmp
        
        detAll = rbind(detAll, cbind( tmp["Site"],tmp["Start"],tmp["End"],tmp["mins"],tmp["sourceType"]) )
        
      }
    }
    
    ## atlanticcod detections ####
    if (inTmp == "atlanticcod" ){
      tmp = read.csv(inFile)
      if (length( colnames(tmp) ) == 3 ){
        colnames(tmp) = c("ISOStartTime","ISOEndTime","Label" )
        if ( nrow(tmp) != 0 )  {
          tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
          tmp$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOEndTime)),   tz = "GMT" )
          tmp$Site = siteOut
          tmp$mins = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "mins" )) )
          tmp$sourceType = inTmp
          
          detAll = rbind(detAll, cbind( tmp["Site"],tmp["Start"],tmp["End"],tmp["mins"],tmp["sourceType"]) )
          rm(tmp)
          
        }
        
      }  
    }
    

  }
  
  # CHECKS ####
  detAll$Day = as.Date(detAll$Start)
  detAll$Dayed = as.Date(detAll$End)
  detAll$season = "unk"
}

# OUTPUT COMBINED DETECTIONS ####
stn = length(unique(detAll$sourceType))
write.csv(detAll , paste0(dirOut,"\\", siteN,"\\", siteN, "_", stn, "-Labels.csv" ) )

# SUMMARY GRAPHIC ####
# tile plot of detection types
detAllt <- detAll[!detAll$sourceType %in% c("NothingIceOK", "AnyOdonto", "AnyDblKnk", "AnyCet", "AnyBel", "AnyAnthro","AnyPinn", "AnyBaleen","AnyBio"),]

ggplot(detAllt, aes(x=Start, xend=End, y=sourceType, yend=sourceType, color = season)) +
  geom_segment() +  theme_bw() + 
  geom_segment(size=8) +
  xlab("")+  ylab("") + 
  ggtitle (paste("Soundscape Labels")) +
  theme(  axis.text.y = element_text(size = 12, colour="black"),
          axis.text.x = element_text(size = 12, colour="black"),
          plot.caption = element_text(size = 8) ,
          plot.title = element_text(size = 16))

# DETECTION TYPES TO INCLUDE ####

## FishChorus detections ####
if (inTmp == "fishchoruses" ){
  detTmp = detFiles[grepl(detTypes[dd], detFiles)] 
  
  tmp = read.csv(detTmp) # head(tmp)
  
  tmp$Start = as.POSIXct(tmp$Start_Time_UTC, format = "%m/%d/%Y %H:%M", tz = "GMT" )
  tmp$End = as.POSIXct(tmp$End_Time_UTC, format = "%m/%d/%Y %H:%M", tz = "GMT" )
  tmp$Site = st
  tmp$Dep  = dpl
  tmp$Yr  = year(tmp$Start )
  tmp$Mth = month(tmp$Start )
  tmp$DurS = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "secs" )) )
  tmp$DurH = tmp$DurS/3600
  tmp$Type = paste0( inTmp, tmp$Chorus_Type, "_bio")  # label as biological
  tmp2 = cbind(empty_column=NA, empty_column=NA, inTmp, tmp[,18:26])
  colnames(tmp2)[1]= "ISOStartTime"
  colnames(tmp2)[2]= "ISOEndTime"
  colnames(tmp2)[3]= "Label"
  detAll = rbind(detAll, tmp2)
  rm(tmp)
} 

## googleai detections ####
if (inTmp == "googleai" ){
  detTmp = detFiles[grepl(inTmp, detFiles)] 
  tmp = read.csv(detTmp) # head(tmp)
  tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
  
  # each hour has a probability -- threshold of .60? prop of hour with google AI
  tmp$ISOEndTime   = tmp$Start + 60*60
  tmp$End   = tmp$Start + 60*60
  tmp$Label = tmp$Proportion.of.positive.GoogleAI.detections
  tmp$Site = st
  tmp$Dep  = dpl
  tmp$Yr   = year(tmp$Start )
  tmp$Mth  = month(tmp$Start )
  tmp$DurS = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "secs" )) )
  tmp$DurH = tmp$DurS/3600
  tmp$Type = paste0( inTmp, "_bio") 
  head(tmp)
  head(detAll)
  # reorder columns
  tmp = ( tmp[,c(1,4,6,3,5,7,8,9,10,11,12,13 )] )
  
  tmp = tmp[tmp$Label > .6, ]
  detAll   = rbind(detAll, tmp)
  rm(tmp)
} 

## plainfinmidshipman detections ####
if (inTmp == "plainfinmidshipman" ){
  detTmp = detFiles[grepl(inTmp, detFiles)] 
  tmp = read.csv(detTmp)
  if (length( colnames(tmp) ) == 3 ){
    colnames(tmp) = c("ISOStartTime","ISOEndTime","Label" )
    tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
    tmp$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOEndTime)),   tz = "GMT" )
    if (tmp$Label[1] != 0 & nrow(tmp) > 1) #no detections just file with start and end time!
    {
      tmp$Site = st
      tmp$Dep  = dpl
      tmp$Yr  = year(tmp$Start )
      tmp$Mth = month(tmp$Start )
      tmp$DurS = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "secs" )) )
      tmp$DurH = tmp$DurS/3600
      tmp$Type = paste0( inTmp, "_anthro") 
      detAll = rbind(detAll, tmp)
    }
    
    rm(tmp)
  }
}

## bocaccio detections ####
if (inTmp == "bocaccio" ){
  detTmp = detFiles[grepl(inTmp, detFiles)] 
  tmp = read.csv(detTmp)
  if (length( colnames(tmp) ) == 3 ){
    colnames(tmp) = c("ISOStartTime","ISOEndTime","Label" )
    tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
    tmp$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOEndTime)),   tz = "GMT" )
    tmp$Site = st
    tmp$Dep  = dpl
    tmp$Yr  = year(tmp$Start )
    tmp$Mth = month(tmp$Start )
    tmp$DurS = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "secs" )) )
    tmp$DurH = tmp$DurS/3600
    tmp$Type = paste0( inTmp, "_bio") 
    detAll = rbind(detAll, tmp)
    rm(tmp)
  }  
}


## blue whale detections ####
if (inTmp== "bluewhale" ) {
  detTmp = detFiles[grepl(inTmp, detFiles)] 
  tmp = read.csv(detTmp)
  
  if ( length( colnames(tmp) ) == 3 ) {
    colnames(tmp) = c("ISOStartTime","ISOEndTime","Label" )
    if (tmp$Label[1] != 0)  {
      tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
      tmp$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOEndTime)),   tz = "GMT" )
      tmp$Site = st
      tmp$Dep  = dpl
      tmp$Yr  = year(tmp$Start )
      tmp$Mth = month(tmp$Start )
      tmp$DurS = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "secs" )) )
      tmp$DurH = tmp$DurS/3600
      tmp$Type = paste0( inTmp, "_bio") 
      detAll = rbind(detAll, tmp)
      rm(tmp)
      
    }
    
  } else if ( length( colnames(tmp) ) == 2 ) {
    # only 2 columns- just start time of the detection....
    colnames(tmp) = c("ISOStartTime","Label" )
    
    if (tmp$Label[1] != 0)  { 
      tmp = as.data.frame ( cbind( tmp$ISOStartTime, tmp$ISOStartTime, tmp$Label) )
      colnames(tmp) = c("ISOStartTime","ISOEndTime","Label" )
      tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
      tmp$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOEndTime)),   tz = "GMT" ) + 10
      tmp$Site = st
      tmp$Dep  = dpl
      tmp$Yr  = year(tmp$Start )
      tmp$Mth = month(tmp$Start )
      tmp$DurS = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "secs" )) )
      tmp$DurH = tmp$DurS/3600
      tmp$Type = paste0( inTmp, "_bio")
      detAll = rbind(detAll, tmp)
    }
    rm(tmp)
    
  }
  else {  rm(tmp) }   
}

