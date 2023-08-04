# Purpose: label and Integrate 1-min hybrid-milli-decade (HMD) data with event based detections
# Event detections = detection periods with start and end time
## (!!! make this a jupter notebook !!!!) ####

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
rm(list=ls()) 

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# DETAILS ####
# Label with specific predefined acoustic scene categories
# See AK labels
# low-frequency- 100- 500 Hz (Kait is 500 Hz, so limiting, why not 1 or 2 kHz)
# bio is very generic
# bio + anthro
# anthro
# unknown?
# Label with  context variables: ice (1-4), wind (1 or 2), AIS  (bring in once acoustic scene analysis)
# this is a generic need in community- is labeling data- wind and event-based detections?
# axiom help with data integration- no just visualization + data products
# show example of wind and what I needed do to get to this

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# DIRECTORIES ####
dirTop = "F:\\SanctSound" # ?? create loop through all manta directories ??
pltf = 0 # change to 1 if you want to plot daily 1-min spectra
dirOut = "F:\\SanctSound\\analysis\\combineFiles_AcousticScene"

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# MANTA HMD DATA ####
# by deployment
inSites = c( "CI04_02", "GR01_01", "HI04_02", "MB02_02", "OC02_02", "PM02_01","SB01_02", "SB03_08")

ver = "manta_9.6.14"

# GENERAL INFORMATION ####
# run this to get summery of detections
for (s in 1:length(inSites)  ) { #  1:length(inSites) s = 1
  
  
  inS = inSites[s] 
  inDir = paste0(dirTop, "//", inS, "//", ver)
  
  #inDir  = choose.dir(default = dirTop , caption = "Site directory with HMD csv files" ) # GR01_01
  inHMD =  list.files(path = inDir, pattern = "MinRes.csv", full.names = T,recursive = T)
  st = sapply(strsplit(basename( inHMD [1] ), "_"), "[[", 1) #site name
  dpl = sapply(strsplit(basename( inHMD[1] ), "_"), "[[", 2) # deployment name
  dirSite = dirname(inHMD[1])
  dirSite = unlist(strsplit(dirSite, '/'))
  dirSite = paste0(dirSite[-length(dirSite)], collapse = '/')
  #ver = strsplit( gsub("^.*[\\]", "", inHMD[1] ),"/" )[[1]] [1]
  
  cat("Processing..." , inS, "-", length(inHMD), " HMD files,", ver)
 
  dirDets = paste0(dirSite,"\\detections\\")
  detFiles = list.files(path = dirDets, pattern = paste(st,dpl,sep="_"), full.names = T, recursive = T)
  detFiles = detFiles[!grepl("1d", detFiles)] #remove 1 day files
  #detFiles = detFiles[!grepl("1h", detFiles)] #remove 1 hour files... too keep humpback AI results
  detFiles = detFiles[!grepl("metadata", detFiles)] #remove metadata
  detFiles = detFiles[!grepl("\\.nc", detFiles)] #remove nc files
  detFiles = detFiles[!grepl("\\.nc", detFiles)] #remove xlz files
  #specific files of interest... !! site dependent!!
  detTypes = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(detFiles)), "_"), "[[", 4) #site name
  cat(st, detTypes,"\n")
}

# COMBINE DETECTIONS ####
for (s in 1:length(inSites)  ) { # length(inSites)
  inS = inSites[s] 
  inDir = paste0(dirTop, "//", inS, "//", ver)
  
  #inDir  = choose.dir(default = dirTop , caption = "Site directory with HMD csv files" ) # GR01_01
  inHMD =  list.files(path = inDir, pattern = "MinRes.csv", full.names = T,recursive = T)
  st = sapply(strsplit(basename( inHMD [1] ), "_"), "[[", 1) #site name
  dpl = sapply(strsplit(basename( inHMD[1] ), "_"), "[[", 2) # deployment name
  dirSite = dirname(inHMD[1])
  dirSite = unlist(strsplit(dirSite, '/'))
  dirSite = paste0(dirSite[-length(dirSite)], collapse = '/')
  #ver = strsplit( gsub("^.*[\\]", "", inHMD[1] ),"/" )[[1]] [1]
  
  dirDets = paste0(dirSite,"\\detections\\")
  detFiles = list.files(path = dirDets, pattern = paste(st,dpl,sep="_"), full.names = T, recursive = T)
  detFiles = detFiles[!grepl("1d", detFiles)] #remove 1 day files
  detFiles = detFiles[!grepl("dolphins", detFiles)] #remove 1 hour files
  #detFiles = detFiles[!grepl("1h", detFiles)] #remove 1 hour files... too keep humpback AI results
  detFiles = detFiles[!grepl("metadata", detFiles)] #remove metadata
  detFiles = detFiles[!grepl("\\.nc", detFiles)] #remove nc files
  #specific files of interest... !! site dependent!!
  detTypes = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(detFiles)), "_"), "[[", 4) #site name
  #cat(st, detTypes,"\n")
  detAll = NULL
  
  cat("Processing..." , inS, "-", length(inHMD), " HMD files,", ver, ': ', detTypes)
  
  ## LOOP through detections ####
  # sonar, plainfinmidshipman, explosions, bocaccio, atlanticcod, VESSEL, blue, mfa, impulse, googleAI
  for (dd in 1:length(detTypes) ) {
    
    inTmp = tolower( detTypes[dd] )
    inTmp
    
    ## VESSEL detections ####
    if (inTmp == "ships" ){
      detTmp = detFiles[grepl(inTmp, detFiles)] 
      tmp = read.csv(detTmp)
      colnames(tmp) = c("ISOStartTime","ISOEndTime","Label" )
      if (tmp$Label[1] != "NoShip") {
        tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
        tmp$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOEndTime)),   tz = "GMT" )
        tmp$Site = st
        tmp$Dep  = dpl
        tmp$Yr  = year(tmp$Start )
        tmp$Mth = month(tmp$Start )
        tmp$DurS = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "secs" )) )
        tmp$DurH = tmp$DurS/3600 
        tmp = tmp[tmp$Label == "ship", ] # head(VD)
        tmp$Type = paste0( inTmp, "_anthro") # head(VD)
        detAll = rbind(detAll, tmp)
        
        rm(tmp)
      }
    }
    
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
    
    ## atlanticcod detections ####
    if (inTmp == "atlanticcod" ){
      detTmp = detFiles[grepl(inTmp, detFiles)] 
      tmp = read.csv(detTmp)
      if (length( colnames(tmp) ) == 3 ){
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
    
  }
  
  # ## CHECK MATCHES WITH HMD ####
  # uDays = unique( as.Date ( detAll$ISOStartTime) )
  # dys = as.Date ( gsub (".csv","", sapply(strsplit( basename( inHMD ), "_"), "[[", 4) ), format="%Y%m%d" )
  # i = which( dys == uDays[1])
  # inFile = inHMD[i]
  # tst = read.csv(inFile) 
   
  #_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
  ## MERGE DETS with HMD ####
  HMDdet = NULL
  ck2 = NULL
  utypes = unique(detAll$Type)
  HMDcheck = NULL
  
  for (ii in 1:length(inHMD)){ #loop through daily files
    
    #read in daily file
    inFile = inHMD[ii]
    inHMDcsv = read.csv(inFile) # basename( inFile )
    ck = 1440 - dim(inHMDcsv)[1]
    dy = as.Date ( gsub (".csv","", sapply(strsplit( basename( inFile ), "_"), "[[", 4) ), format="%Y%m%d" )
    
    colnames(inHMDcsv)[1] = "dateTime"
    inHMDcsv$dateTime = as.POSIXct(   inHMDcsv$dateTime, format = "%d-%b-%Y %H:%M:%S" , tz = "GMT" ) # Date format: format the date (? will netCDF files be the same?)
    fq = as.numeric(as.character( gsub("X","", colnames(inHMDcsv[3:ncol(inHMDcsv)] )) ) ) # Frequency range: truncate to 100-2000 Hz
    str = which(fq == 100)+2      #  colnames(inHMDcsv)[st]
    ed = which(fq == 1997.6)+2   #  colnames(inHMDcsv)[ed]
    
    # CHECK and remove rows that split a given minute into sections
    # Check: length( unique( inHMDcsv$dateTime) ) # they are all unique!! wtf
    # extra rows happen when a minute has extra second 61 seconds for a given minute so splits into different rows
    # solution: only keep 00 seconds for a specific minute
    inHMDcsv$HR  = hour(inHMDcsv$dateTime)
    inHMDcsv$MIT = minute(inHMDcsv$dateTime)
    inHMDcsv$SEC = second(inHMDcsv$dateTime)
    
    #remove any minutes that are not at 00 seconds
    iextra   = which( inHMDcsv$SEC == 0 ) # data to keep
    
    #HMDcheck  = rbind( HMDcheck, cbind(basename(inFile), ck, nrow(inHMDcsv), length(iextra) - nrow(inHMDcsv) ) ) 
    inHMDcsv2 = inHMDcsv[iextra ,]
    HMDcheck  = rbind( HMDcheck, cbind(basename(inFile), ck, nrow(inHMDcsv), length(iextra) - nrow(inHMDcsv), nrow(inHMDcsv2) ) ) 
    
    #truncate HMD data
    inHMDdata = as.data.frame( inHMDcsv2[, c(1, str:ed )] )
    fq = as.numeric(as.character( gsub("X","", colnames(inHMDdata[2:ncol(inHMDdata)] )) ) ) # Frequency range: truncate to 100-2000 Hz
    rm(ed,str, inHMDcsv,inHMDcsv2)
    
    # (optional) plots spectra
    if (pltf == 1) {
      medSPLm = reshape::melt (inHMDdata, id.vars = c("dateTime"),  measure.vars = colnames(inHMDdata)[2:ncol(inHMDdata)] )
      colnames( medSPLm)  = c("date", "Fq", "SPL")
      medSPLm$Fq = as.numeric(as.character( gsub("X","", medSPLm$Fq ) ) ) #head(medSPLm)
      
      ggplot(medSPLm, aes(x = Fq, y=SPL, group = date ) ) +
        geom_line(alpha = .2 ) + 
        scale_x_log10() +
        ylab("1-min HMD")+ xlab("Frequency (Hz)")+
        theme_minimal() +
        ggtitle(paste0( st, " on ", dy) ) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    
    #loop through detections to find all all minutes that match the detection period
    inHMDdata$Dets = 0
    inHMDdata$Type = "none"
    
    # not all detections as labeled because only 1-day, not whole deployment
    for (dd in 1: nrow(detAll) )
    {
      
      # find all TOLs rows that fall with the detection period
      if ( detAll$DurS [dd] <= 60 )  # if detection is less than a minutes, need different logic... because only one row of data
      {
       ttime = as.POSIXct(  format ( detAll$Start[dd], "%Y-%m-%d %H:%M:00" )  , format = "%Y-%m-%d %H:%M:%S" , tz = "GMT")
        
       idx =  which( inHMDdata$dateTime  == ttime  )
        
      } else {
        idx =  which( inHMDdata$dateTime  >= detAll$Start[dd] & inHMDdata$dateTime + 60 < detAll$End[dd] )
      }
      
      if (length(idx) > 0 ) 
      {
        inHMDdata$Dets[idx] =  inHMDdata$Dets[idx] + 1  #can be overlapping because all detections!
        inHMDdata$Type[idx] =  paste( inHMDdata$Type[idx], detAll$Type[dd] , sep = ";") # keep track of types
      }
      
    }

   
   #ck2 = rbind(ck2, c(as.character( dy), ck,length( unique( inHMDdata$FD ) ), length( unique( inHMDdata$VD ) ) ))
    
    cat("Processing... ",st," on " ,as.character( dy), "[", ii, " of ", length(inHMD)," days ]", ck, " minutes removed","\n" )
    cat("Detection types present: ", length( unique( inHMDdata$Type ) )-1 ,"\n" )
    
    #_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
    #ACOUSTIC SCENE LABELS ####
    inHMDdata$Category = "Ambient"
    inHMDdata$Bio = 0
    inHMDdata$Bio[grepl("bio", inHMDdata$Type)] = "1" # rows with bio
    inHMDdata$Ant = 0
    inHMDdata$Ant[grepl("anthro", inHMDdata$Type)] = "1" # rows with bio
    
    inHMDdata$Category[inHMDdata$Ant > 0  & inHMDdata$Bio > 0] = "Bio+Anthro"
    inHMDdata$Category[inHMDdata$Ant > 0  & inHMDdata$Bio == 0] = "Anthro"
    inHMDdata$Category[inHMDdata$Ant == 0 & inHMDdata$Bio > 0 ] = "Bio"
    #as.data.frame(colnames((inHMDdata) ))
    
    #_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
    #WRITE OUT DAILY FILES ####
    inTmp = gsub(".csv","",basename(inFile))
    write.csv(inHMDdata , paste0(dirOut,"\\", inTmp, "_LFAS.csv" ) )
    
    HMDdet = rbind(HMDdet, inHMDdata)  # all data in one file--- way to big!
  } ## !! end a daily loop
  
  colnames(HMDcheck) = c("FileName","MissingMins","secondsInFile_all","MinsRemoved","secondsInFile")
  write.csv(HMDcheck , paste0(dirOut,"\\", st,"_",dpl, "_HMDcheck_", ver,".csv" ) )
  
  DC = Sys.Date()
  # as.data.frame( HMDdet %>% group_by(Category) %>% tally() )
  save(HMDdet, file = paste0(dirOut, "\\HMDdetLF_", st, "_", dpl, DC, "_", ver, ".Rda") )
  
}


unique( HMDdet$Type )
unique( HMDdet$Bio )
unique( HMDdet$Category )
