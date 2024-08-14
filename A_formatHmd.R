# Formate/trim HMD files (Rdat)
# user decides on detection types and frequency range
# one site at a time

# INPUT
#directory of NCEI netCDF HMD files

# OUTPUT
#HMD in Rdat with specific frequency range data (Rdat)

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
siteN = "SB03"
siteN = "CaseStudy2"
siteN = "ESONS"
siteN = "AU_CH01-all"
target_valueIn = 4 # change this to the value you're searching for in quality matrix
frq_range = c(100, 1997.6)
LB = "LF" #what label do you want to indicate on the ouutput file, LF = low frequency
DC = Sys.Date()
flagHMDall = 0

# DIRS ####
gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
dirOut =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS" )

# HMD FILES ####
if (siteN == "AU_CH01") {
  inDir   = paste0(  "F:\\SoundCoop\\hmd_downloadedGCP\\", siteN, "\\ALL" )
  inFiles = list.files( inDir, pattern = "MinRes", full.names = T, recursive = T)
} else if (siteN == "SB03"){
  inDir   = paste0(  "F:\\SoundCoop\\hmd_downloadedGCP\\", siteN )
  inFiles = list.files( inDir, pattern = "MinRes", full.names = T, recursive = T)
} else if (siteN == "subSet_CaseStudy2"){
  inDir   = paste0(  "F:\\SoundCoop\\hmd_downloadedGCP\\", siteN )
  inFiles = list.files( inDir, pattern = "MinRes", full.names = T, recursive = T)
  
} else if (siteN == "CaseStudy2" | siteN == "ESONS"){
  inDir   = paste0(  "F:\\SoundCoop\\hmd_downloadedGCP\\", siteN )
  inDirs = list.dirs(inDir)
  inDirs
  #need to loop through directorys... create a loop!
  inFiles = list.files( inDirs[1], pattern = "MinRes", full.names = T, recursive = T)
  inFiles
} else if (siteN == "AU_CH01-all"){
  inDir   = paste0(  "F:\\SoundCoop\\hmd_downloadedGCP\\", siteN )
  inDirs = list.dirs(inDir)
  #create subfolder for each year...and move files
  inFiles = list.files( inDirs[1], pattern = "MinRes", full.names = T, recursive = T)
  years = unique( substr( sapply(strsplit(basename(inFiles),"_" ),`[`,4), 1,4) )
  setwd(inDirs)
  for (year in years) {
    dir_name <- as.character(year)
    if (!dir.exists(dir_name)) {  # Check if the directory already exists
      dir.create(dir_name)
      cat("Created directory:", dir_name, "\n")
    } else {
      cat("Directory already exists:", dir_name, "\n")
    }
  }
  
  # Move files to corresponding directories
  for (file in inFiles) {
    # Extract year from the filename
    file_year <- substr(sapply(strsplit(basename(file), "_"), `[`, 4), 1, 4)
    
    # Check if the year is within the specified range
    if (file_year %in% as.character(years)) {
      # Construct the target directory path
      target_dir <- file.path(file_year)
      
      # Move the file
      file.rename(file, file.path(target_dir, basename(file)))
      cat("Moved file:", file, "to", file.path(target_dir, basename(file)), "\n")
    } else {
      cat("Skipping file:", file, "due to year not in range\n")
    }
  }
  
  inDirs = list.dirs(inDir)
  #yy = 5
  #inFiles = list.files( inDirs[yy], pattern = "MinRes", full.names = T, recursive = T)
}

for (yy in 6 :length(inDirs)){
  
  inFiles = list.files( inDirs[yy], pattern = "MinRes", full.names = T, recursive = T)
  
  # PROCESS FILES ####
  HmdTrim = NULL
  
  for(fil in 1:length(inFiles)) { # fil=1
    split_string = str_split(basename(inFiles[fil]), "_")[[1]]
    site =  split_string[1]
    
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
    
    ## WRITE out file ####
    if (flagHMDall == 1 ) {
      result <- str_match(basename(inFiles[fil]) , "^(.*)_(\\d{8})_(.*)\\.nc$")
      # write out as netCDF HMD+
      write.csv(inHMDdata , paste0(dirOut,"\\", result[2], "_", result[3], "_", result[4],"_Trim.csv" ) )
    }
    
    ## TIME CHECK ####
    ck = dim(inHMDdata)[1] # how many minutes in the daily file
    
    colnames(inHMDdata)[1] = "dateTime"
    inHMDdata$dateTime = as.POSIXct(   inHMDdata$dateTime, format = "%Y-%m-%d %H:%M:%S" , tz = "GMT" ) 
    inHMDdata$HR  = hour(inHMDdata$dateTime)
    inHMDdata$MIT = minute(inHMDdata$dateTime)
    inHMDdata$SEC = second(inHMDdata$dateTime)
    inHMDdata$site =site
    
    #remove any minutes that are not at 00 seconds
    iextra   = which( inHMDdata$SEC == 0 ) # data to keep
    inHMDdata = inHMDdata[iextra ,]
    
    ## FREQUENCY OF INTEREST ####
    str = which(colnames(inHMDdata)== frq_range[1] )     
    ed  = which(colnames(inHMDdata)== frq_range[2] )  # colnames(inHMDdata)[ed]
    st  =  which(colnames(inHMDdata)== "site" )
    inHMDdata = as.data.frame( inHMDdata[ , c(1, st, str:ed) ] )
    
    ## COMBINE Rdata output ####
    cat("Processing... ", fil, " of", length(inFiles), " for ", as.character(tmpDate), "(", ck, "out of 1440 mins)", "\n")
    
    HmdTrim= rbind(HmdTrim, inHMDdata)
    
  }
  
  
  # OUTPUTS ####
  ## Rdat file ####
  if (siteN == "CaseStudy2" | siteN == "ESONS"){
    save(HmdTrim, file = paste0(inDir, "\\", site, "_Hmd_",LB, "_", DC) )
  }else if (siteN == "AU_CH01") {
    save(HmdTrim, file = paste0(dirOut, "\\", siteN, "_Hmd_",LB, "_", DC) ) 
  }else if (siteN == "AU_CH01-all") {
    save(HmdTrim, file = paste0(inDir, "\\", siteN,"-", years[yy-1], "_Hmd_",LB, "_", DC) ) 
  }
  
}











