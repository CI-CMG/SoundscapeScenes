# HMDs with Detections categories

# some initial plots, assign acoustic scenes types, save one large file

# INPUT: output of 1_HMDwithDets_CH01.R

rm(list=ls()) 

# LIBRARIES ####
library(lubridate)
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggmosaic)

# HMD+ ####
siteN = "AU_CH01"
gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
dirIn =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS\\",siteN )
inFiles = list.files(dirIn, pattern = "_Dets.csv", full.names = T )

AS = NULL
for (ff in 1:length(inFiles)){ # ff = 1
  cat("Processing...", basename( inFiles[ff]) , "(", ff, " of ", length(inFiles) , ")\n" ) 
  
  inHMDdata = read.csv(inFiles[ff])
  inHMDdata = inHMDdata[,2:ncol(inHMDdata)]
  #colnames(inHMDdata[995:1017])
  
  detection_columnsNames <- ( grep("Any|Nothing", colnames(inHMDdata), value = TRUE) )
  detection_columns = ( grep("Any|Nothing", colnames(inHMDdata)) )
 
  HMD_columns_Names <- grep("X", colnames(inHMDdata), value = TRUE)
  HMD_columns = ( grep("X", colnames(inHMDdata)) )
  season_columns = ( grep("season", colnames(inHMDdata)) )
  
  #TIME CHECK
  ck = 1440 - dim(inHMDdata)[1]
  dy = as.Date ( gsub (".csv","", sapply( strsplit( basename( inFiles[ff] ), "_"), "[[", 4) ), format="%Y%m%d" )
  colnames(inHMDdata)[1] = "dateTime"
  inHMDdata$dateTime = as.POSIXct(   inHMDdata$dateTime, format = "%Y-%m-%d %H:%M:%S" , tz = "GMT" ) 
  inHMDdata$HR  = hour(inHMDdata$dateTime)
  inHMDdata$MIT = minute(inHMDdata$dateTime)
  inHMDdata$SEC = second(inHMDdata$dateTime)
  #remove any minutes that are not at 00 seconds
  iextra   = which( inHMDdata$SEC == 0 ) # data to keep
  inHMDdata = inHMDdata[iextra ,]
  #remove any minutes that are not full 60 seconds-- this column does not exist for this dataset???
  #ikeep = which( inHMDdata2[,2] > 58 ) # which( inHMDcsv2[,2] < 59)
  #inHMDcsv2 = inHMDdata2[ikeep ,]
  # dupTimes = sum( duplicated(inHMDcsv2$dateTime))
  # HMDcheck  = rbind( HMDcheck, cbind(basename(inFile), ck, nrow(inHMDcsv), nrow(inHMDcsv2),dupTimes ) ) 
  
  #FREQUENCY OF INTEREST
  fq  = as.numeric(as.character( gsub("X","", HMD_columns_Names )) )  # Frequency range: truncate to 100-2000 Hz
  str = which( colnames(inHMDdata) == "X100")      #   colnames(inHMDdata)[str]
  ed  = which( colnames(inHMDdata) == "X1997.6")    #  colnames(inHMDdata)[ed]
  inHMDdata = as.data.frame( inHMDdata[, c(1, str:ed, detection_columns, season_columns)] )
  #inHMDdata =  inHMDdata2 
  
  #ACOUSTIC SCENE LABELS ####
  # head(inHMDdata)
  inHMDdata$Category = "ambient"
  inHMDdata$Bio = 0
  # AnyBaleen AnyBeard AnyBel AnyBio AnyBow AnyCet AnyDblKnk AnyOdonto AnyPinn AnyRib  AnyWalrus
  inHMDdata$Bio[inHMDdata$AnyBio > 0] = 1
  # AnyAnthro, AnyVess
  inHMDdata$Ant = 0
  inHMDdata$Ant[inHMDdata$AnyAnthro > 0] = 1 # sum(inHMDdata$AnyAnthro)
  inHMDdata$Category[inHMDdata$Ant > 0  & inHMDdata$Bio > 0] = "bio+anthro"
  inHMDdata$Category[inHMDdata$Ant > 0  & inHMDdata$Bio == 0] = "anthro"
  inHMDdata$Category[inHMDdata$Ant == 0 & inHMDdata$Bio > 0 ] = "bio"
  #unique(inHMDdata$Category)
  
  #combine into one file- rdata output
  AS = rbind(AS, inHMDdata)
}

AS$mth = month(AS$dateTime)
tal = as.data.frame( AS %>% group_by(Category, mth) %>% tally() )
talALL = as.data.frame( AS %>% group_by(Category) %>% tally() )
talSea = as.data.frame( AS %>% group_by(Category, season) %>% tally() )
write.csv(AS, paste0(dirIn, "\\", siteN, "_HmdDetsAS.csv") ) 
save(AS, file = paste0(dirIn, "\\", siteN, "_HmdDetsAS") ) 
