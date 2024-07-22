#Read in and process SanctSound data products
rm(list=ls()) 

# Downloaded data from here:


siteN  = "MB01"
dirTop = "F:\\SanctSound" 

# INPUT FILES ####
inFiles = list.files(path = dirTop, pattern = siteN, full.names = T, recursive = T)
inFiles = inFiles[!grepl("/analysis/", inFiles)] #remove analysis
##  Sound Levels ####
filesTOL = inFiles[grepl("TOL_1h", inFiles)] 
filesBB = inFiles[grepl("BB_1h", inFiles)] 
## Detections  ####
filesShip = inFiles[grepl("hips", inFiles)] 
filesDet = inFiles[!grepl("1d", inFiles)] #remove 1 day files
filesDet = filesDet[!grepl("1h", filesDet)] #remove 1 hr files
filesDet = filesDet[!grepl("\\.nc", filesDet)] #remove nc files
filesDet = filesDet[!grepl("metadata", filesDet)] #remove metadata
filesDet = filesDet[!grepl("DataQualityReport", filesDet)] #remove metadata
filesDet = filesDet[!grepl("hips", filesDet)] #remove metadata
basename(filesDet)
detTypes = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filesDet)), "_"), "[[", 4) 
## #AIS  ####
##? NEED May 2021- Jan 2022 ####
file.list = list.files(path = dirTop, pattern = paste0("AIS_", substr(siteN, start = 1, stop = 2), "_2018"), full.names = T, recursive = T)
file_info <- file.info(file.list) 
fileAIS <- rownames(file_info)[which.max(file_info$mtime)]# Find the most recent file
AIS = read.csv(fileAIS)
AISall = AIS[ AIS$LOC_ID == siteN,] 

# READ IN SOUND LEVELS ####
TOLall = NULL
for (ii in 1:length(filesTOL)) { # ii = 4
  
  tmpFile = filesTOL [ii]
  typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
  
  if (typ == "csv") {
    tmp = read.csv(tmpFile)
    colnames(tmp)[1]= "ISOTime"
    tmp$dateTime = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOTime)), tz = "GMT" )
    TOL_cols = ( grep("TOL", colnames(tmp)) ) 
    dcolm = ( grep("dateTime", colnames(tmp)) ) 
    tmp = as.data.frame( tmp[ c(dcolm, TOL_cols ) ] )
    column_names <- gsub("\\.", "_", colnames(tmp))
    colnames(tmp) = column_names
    
    TOLall = rbind(TOLall, tmp)
    
  } else if (typ == "nc"){
    nc = nc_open(tmpFile)
    f =  paste0( "TOL_", t( as.data.frame( ncvar_get(nc, "frequency")   ) ) )
    myTOL <- ( ncvar_get(nc, "sound_pressure_levels")   )
    myTOL <- as.data.frame( myTOL  )
    myTOL <- as.data.frame( t( as.data.frame( myTOL  ) ) )
    
    time <- ncvar_get(nc, "time_stamp")
    myTime <- as.data.frame( as.POSIXct(time, origin = "1970-01-01", tz = "UTC") )
    
    f <- gsub("\\.", "_", f)
    tmp = cbind(myTime,myTOL)
    colnames(tmp) = c("dateTime",f)
    nc_close(nc)
    TOLall = rbind(TOLall, tmp)
  }
  
  
}

# READ IN BB SOUND LEVELS ####
BBall = NULL
for (ii in 1:length(filesBB)) { # ii = 4
  
  tmpFile = filesBB [ii]
  typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
  
  if (typ == "csv") {
    tmp = read.csv(tmpFile)
    colnames(tmp)[1]= "ISOTime"
    tmp$dateTime = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOTime)), tz = "GMT" )
    TOL_cols = ( grep("BB", colnames(tmp)) ) 
    dcolm = ( grep("dateTime", colnames(tmp)) ) 
    tmp = as.data.frame( tmp[ c(dcolm, TOL_cols ) ] )
    column_names <- gsub("\\.", "_", colnames(tmp))
    colnames(tmp) = column_names
    
    BBall = rbind(BBall, tmp)
    rm(tmp)
    
  } else if (typ == "nc"){
    nc = nc_open(tmpFile)
   
    myTOL <- as.data.frame ( ncvar_get(nc, "sound_pressure_levels")   )
    time <- ncvar_get(nc, "time_stamp")
    myTime <- as.data.frame( as.POSIXct(time, origin = "1970-01-01", tz = "UTC") )
    
    tmp = cbind(myTime,myTOL)
    colnames(tmp) = c("dateTime","BB_20_24000")
    nc_close(nc)
    BBall = rbind(BBall, tmp)
  }
  
  
}

# READ DETECTIONS ####
shipsAll = NULL
siteD = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filesShip)), "_"), "[[", 3)
for (dd in 1:length(filesShip) ) { # dd = 4
  
  siteOut = paste0( siteN, "_", siteD[dd])
  inFile = filesShip[dd]
  typ = sapply( strsplit(basename(inFile), "[.]"), "[[",2)
  
  if (typ == "csv") {
    tmp = read.csv(inFile)
    colnames(tmp) = c("ISOStartTime","ISOEndTime","Label" )
    if (tmp$Label[1] != "NoShip") {
      
      tmp$Site = siteOut
      tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
      tmp$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOEndTime)),   tz = "GMT" )
      
      tmp$mins = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "mins" )) )
      tmp$sourceType = "vessel"
      
      shipsAll = rbind(shipsAll, cbind( tmp["Site"],tmp["Start"],tmp["End"],tmp["mins"],tmp["sourceType"]) )
      rm(tmp)
    }
  } else if (typ == "nc") {
    nc = nc_open(inFile)
    tmp = NULL
    start_time = as.data.frame( as.POSIXct(( ncvar_get(nc, "start_time")/1000   ), origin = "1970-01-01", tz = "UTC") )
    end_time   = as.data.frame( as.POSIXct(( ncvar_get(nc, "end_time")/1000   ),   origin = "1970-01-01", tz = "UTC") )
    tmp$Start = start_time
    tmp$End = end_time
    tmp = as.data.frame(tmp)
    colnames(tmp) = c("Start", "End")
    tmp$mins = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "mins" )) )
    tmp$Site = siteOut
    tmp$sourceType = "vessel"
    tmp$ship  = as.data.frame ( ncvar_get(nc, "ships_presence")   )
    nc_close(nc)
    shipsAll = rbind(shipsAll, cbind( tmp["Site"],tmp["Start"],tmp["End"],tmp["mins"],tmp["sourceType"]) )
    rm(tmp)
    
  }
}


