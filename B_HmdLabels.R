# Combine label data with HMD files create HMD+ data (Rdat)
#user decides on detection types 
#one site at a time

# INPUTS
#output of A_formatLabels.R (csv)
#output of A_formatHmd.R (Rdat)
#wind data 
#AIS data

# OUTPUT
#HMD+ with specific frequency range data- column for each detection type (Rdat)

# NEXT
#write out a new netCDF file with the detections for other programs

rm(list=ls()) 

# LIBRARIES ####
library(tidyverse)
library(readxl)
library(dplyr)
library(ncdf4)

# PARAMS ####
siteN = "AU_CH01"
# siteN = "SB03"
flagHMDall = 0 # make 1 if you want to write out HMD+ files with all the data
target_valueIn = 4 # change this to the value you're searching for in quality matrix
frq_range = c(100, 1997.6)
flageRemove = 1 #remove days without seasonal data= 1
HmdDets = NULL
LB = "LF" #what label do you want to indicate on the ouutput file, LF = low frequency
DC = Sys.Date()
windflag = 0 
aisflag = 0
# DIRS ####
gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
dirOut =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS" )

# HMD FILES ####
if (siteN == "AU_CH01") {
  inDir   = paste0(  "F:\\SoundCoop\\hmd_downloadedGCP\\", siteN, "\\ALL" )
  inFiles = list.files( inDir, pattern = "MinRes", full.names = T, recursive = T)
} else {
  inDir   = paste0(  "F:\\SoundCoop\\hmd_downloadedGCP\\", siteN )
  inFiles = list.files( inDir, pattern = "MinRes", full.names = T, recursive = T)
}

# DETECTION FILE ####
inDir = paste0( dirOut, "\\",siteN )
inFile  = list.files( inDir, pattern = "-Labels.csv", full.names = T, recursive = T)
detAll = read.csv(inFile)
uDets = unique(detAll$sourceType)
# some issues with matching when detections are 

# WIND ####
inDirW = paste0( gdrive, "SoundCoop_AcousticScene\\DATA\\wind" )
inFilesW = list.files( inDirW, pattern = "env", full.names = T)
inFilesW = inFilesW[grepl(siteN, inFilesW)]
if (length( inFilesW) != 0 ) {
  windflag = 1
  WINDdata = read.csv(inFilesW)
  # as.data.frame(colnames( WINDdata) )
  ixd = which(!is.na(WINDdata$wind_speed))
  WINDdata = WINDdata[ixd, ]
  WINDdata$time = gsub("[+]00:00", "", WINDdata$time )
  WINDdata$dateTime = as.POSIXct( WINDdata$time, format = "%Y-%m-%d %H:%M:%S" , tz = "GMT")
  WINDdata = WINDdata[,-1]
}

# AIS ####
inDirW = paste0( gdrive, "SoundCoop_AcousticScene\\DATA\\ais" )
inFilesA = list.files(path = inDirW, pattern = "transit", full.names = T,recursive = F)
tmp = read.csv(inFilesA)
loc_ids = unique(tmp$loc_id)
AIStran = NULL
if ( siteN %in% loc_ids ) {
  aisflag = 1
  tmp = tmp[ tmp$loc_id == siteN,]
  tmp$Start = as.POSIXct( gsub("[+]00", "", tmp$start_time_utc), tz = "GMT" ) 
  tmp$End   = as.POSIXct( gsub("[+]00", "", tmp$end_time_utc), tz = "GMT" ) 
  AIStran = rbind(AIStran, tmp)
  
}
rm(tmp)

# COMBINE HMD with Dets ####
for(fil in 1:length(inFiles)) { # fil=1
  
  ## READ in HMD netCDF files ####
  nc <- nc_open(inFiles[fil])
  f =  t( as.data.frame( ncvar_get(nc, "frequency")   ) )
  myPSD <- ( ncvar_get(nc, "psd")   )
  qualityMat <- ncvar_get(nc, "quality_flag")   
  target_value <- target_valueIn  
  indices <- which(qualityMat == target_value, arr.ind = TRUE)
  row_indices <- indices[, 1]
  col_indices <- indices[, 2]
  myPSD[row_indices, col_indices] <- NA
  myPSD <- as.data.frame( myPSD  )
  time <- ncvar_get(nc, "time")
  myTime <- as.data.frame( as.POSIXct(time, origin = "1970-01-01", tz = "UTC") )
  myPSD <- t( as.data.frame( myPSD  ) )
  inHMDdata = cbind(myTime,myPSD)
  colnames(inHMDdata) = c("dateTime",f)
  nc_close(nc)
  tmpDate = unique( as.Date(inHMDdata$dateTime) )
  
  ## ADD new columns with all zeros for detections ####
  # set default values
  inHMDdata$season <- "notFilled"
  uDets = unique(detAll$sourceType)
  for (i in 1: length(uDets) ) {
    new_col_name <- c(uDets[i])
    inHMDdata[[new_col_name]] <- 0  # is setting to zero the right approach?
  }
  
  ## TRIM detAll to only fall within the HMD data to speed up this loop ####
  # only goes by the start date... 
  idxDay = which( detAll$Day  == tmpDate )
  detAlltmp = detAll[idxDay,]
  
  ## FIND detections that fall in HMD each minute ####
  for (dd in 1:nrow(detAlltmp)  ) { # dd = 2 ()
    
    #find which "HMD rows" the detection corresponds to
    idx =  which( inHMDdata$dateTime  >= detAlltmp$Start[dd] & inHMDdata$dateTime <= detAlltmp$End[dd] )
    inHMDdata$dateTime[idx]
    
    #tally the detections for that minute of data 
    idc = which(detAlltmp$sourceType[dd] == colnames(inHMDdata)) 
    # colnames(inHMDdata)[idc], find the right column for the detection type
    inHMDdata[idx,idc] =  inHMDdata[idx,idc] + 1
    
    #add season to the output-- same for the whole day
    inHMDdata$season[idx] = unique(detAlltmp$season)
    #why are there rows with no label- because not detections??
   
  
    #  inHMDdata$dateTime2 = inHMDdata$dateTime 
    
    
  }
  
  #WHY "not Filled"
  #There are minutes in this day without any detections- b/c all zeros, 
  #so no detections fell in those minutes
  #which(inHMDdata$season == "notFilled") 
  #idxNF = which(inHMDdata$season == "notFilled")
  #time_value =  inHMDdata$dateTime2 [idxNF[9]] #this is a minute with no label... check detALL again
  #Check if the time falls within any row with start and end times
  #time_within_range <- any(time_value >= detAll$Start & time_value <=  detAll$End)
  
  ## WRITE out file ####
  if (flagHMDall == 1 ) {
    result <- str_match(basename(inFiles[fil]) , "^(.*)_(\\d{8})_(.*)\\.nc$")
    # write out as netCDF HMD+
    write.csv(inHMDdata , paste0(dirOut,"\\", result[2], "_", result[3], "_", result[4],"_Dets.csv" ) )
  }
  
  ## COLUMNN NAMES ####
  season_columns = ( grep("season", colnames(inHMDdata)) ) 
  detection_columns = which( names(inHMDdata) %in% uDets  )
  HMD_columns = which( names(inHMDdata) %in% f  )

  ## TIME CHECK ####
  ck = 1440 - dim(inHMDdata)[1]
  dy = as.Date ( gsub (".csv","", sapply( strsplit( basename( inFiles[fil] ), "_"), 
                                          "[[", 4) ), format="%Y%m%d" )
  colnames(inHMDdata)[1] = "dateTime"
  inHMDdata$dateTime = as.POSIXct(   inHMDdata$dateTime, format = "%Y-%m-%d %H:%M:%S" , tz = "GMT" ) 
  inHMDdata$HR  = hour(inHMDdata$dateTime)
  inHMDdata$MIT = minute(inHMDdata$dateTime)
  inHMDdata$SEC = second(inHMDdata$dateTime)
  #remove any minutes that are not at 00 seconds
  iextra   = which( inHMDdata$SEC == 0 ) # data to keep
  inHMDdata = inHMDdata[iextra ,]
  
  ## FREQUENCY OF INTEREST ####
  str = which(colnames(inHMDdata)== frq_range[1] )     
  ed  = which(colnames(inHMDdata)== frq_range[2] )  # colnames(inHMDdata)[ed]
  inHMDdata = as.data.frame( inHMDdata[ , c(1, str:ed, detection_columns, season_columns) ] )

  ## COMBINE Rdata output ####
  cat("Processing... ", fil, " of", length(inFiles), "\n")
  HmdDets = rbind(HmdDets, inHMDdata)
  
}

# CHECKS ###
df = HmdDets #saves a good copy to go back to! HmdDets = df
HmdDets$dy  = as.Date(HmdDets$dateTime)
talSeason   = as.data.frame( HmdDets %>% group_by(season) %>% tally() )
talSeason

## FILL in season dates ####
useason = unique(HmdDets$season)
HmdDets$season2 = "unk"
for (ss in 2:length(useason)) {
  # all unique days in specific season
  dates_to_match = unique( HmdDets$dy[HmdDets$season == useason[ss] ] ) 
  HmdDets$season2[HmdDets$dy %in% dates_to_match] = useason[ss]
  cat("HMD Dates for ", useason[ss],"days = ", length(dates_to_match), as.character( min(dates_to_match)), 
      as.character( max(dates_to_match)),"\n" )
}
talSeason   = as.data.frame( HmdDets %>% group_by(season2) %>% tally() )
talSeason

## removes days without data ####
if (flageRemove == 1) { HmdDets = HmdDets[HmdDets$season2 != "unk", ]}

# OUTPUTS ####
str  = which(colnames(HmdDets) == frq_range[1] ) 
ed   = which(colnames(HmdDets)  == frq_range[2] ) 
dets = which(colnames(HmdDets) %in% uDets )
season_columns = which(colnames(HmdDets) == "season2" ) 
colnames(HmdDets)[season_columns] = "season"
HmdDetsT = as.data.frame( HmdDets[, c(1, str:ed, dets, season_columns ) ] )
HmdDets = HmdDetsT # in case I mess up

## SUMMARY PLOT ####
# spectra by unique labels
numeric_columns = grep("^\\d", names(HmdDets) )  
hix = names(HmdDets)[numeric_columns]
useason = unique(HmdDets$season)
dfT = NULL
for (s in 1:length(useason)) {
  tmpD  = HmdDets %>% filter(season == useason[s])
  tmpP = tmpD %>% gather(key, value, numeric_columns) %>% group_by(key) %>% 
    dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                     med.x  = quantile(value, probs = 0.5),
                     upper.x = quantile(value, probs = 0.75))
  tmpP$Category = useason[s]
  dfT = rbind(dfT, tmpP) 
  rm(tmpD, tmpP)
}
ggplot(dfT, aes(x=as.numeric( as.character(key) ) , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  facet_wrap(~Category) +
  scale_x_log10() +  ylab("1-min PSD median") + xlab("HMD Frequency (Hz)")+
  ylim(c(50,95)) +   theme_bw()+ 
  ggtitle (paste("Soundscape Metrics by Seasons")) +
  theme(text = element_text(size = 16) )


## Rdat file ####
save(HmdDets, file = paste0(inDir, "\\", siteN, "_HmdLabels_",LB, "_", DC) )



