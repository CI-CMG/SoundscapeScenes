#Read in and process SanctSound data products
rm(list=ls()) 

library(ncdf4)
library(dbplyr)
library(tidyr)
library(lubridate)
library(reshape)
library(ggplot2)

# Downloaded data from here:
siteN  = "MB01"
dirTop = "F:\\SanctSound" 
specInterest = "humpbackwhale"

# INPUT FILES ####
inFiles = list.files(path = dirTop, pattern = siteN, full.names = T, recursive = T)
inFiles = inFiles[!grepl("/analysis/", inFiles)] #remove analysis
##  Sound Levels ####
filesTOL = inFiles[grepl("TOL_1h", inFiles)] 
filesBB = inFiles[grepl("BB_1h", inFiles)] 

## Detections  ####
filesShip = inFiles[grepl("hips", inFiles)] 
#filesDet = inFiles[!grepl("1d", inFiles)] #remove 1 day files
filesDet = inFiles[!grepl("1h", inFiles)] #remove 1 hr files
#filesDet = filesDet[!grepl("\\.nc", filesDet)] #remove nc files
filesDet = filesDet[!grepl("metadata", filesDet)] #remove metadata
filesDet = filesDet[!grepl("DataQualityReport", filesDet)] #remove metadata
filesDet = filesDet[!grepl("hips", filesDet)] #remove metadata
filesDet = filesDet[grepl(specInterest, filesDet)] #remove metadata
basename(filesDet)
detTypes = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filesDet)), "_"), "[[", 4) 

## #AIS  ####
file.list = list.files(path = paste0(dirTop, "\\analysis\\AIS\\"), pattern = substr(siteN, start=1, stop = 2 ), full.names = T, recursive = F)
file_info <- file.info(file.list) 
#fileAIS <- rownames(file_info)[which.max(file_info$mtime)]# Find the most recent file
AIS = NULL
for (ff in 1:length(file.list) ) {
  tmp =  read.csv(file.list[ff]) 
  max(tmp$DATE)
  AIS = rbind( AIS, tmp)
}
AIS = AIS[ AIS$LOC_ID == siteN,]
# max(as.Date(AIS$DATE, format = "%m/%d/%Y", tz = "GMT" ) )
AIS$DATE = as.Date(AIS$DATE, format = "%m/%d/%Y", tz = "GMT" )

# READ IN SOUND LEVELS ####
TOLall = NULL
for (ii in 1:length(filesTOL)) { # ii = 4
  
  tmpFile = filesTOL [ii]
  typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
  
  if (typ == "csv") {
    tmp = read.csv(tmpFile)
    colnames(tmp)[1]= "ISOTime"
    tmp$dateTime = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOTime)), tz = "GMT" )
    max(tmp$dateTime)
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
    myTime = as.POSIXct( gsub(".000Z", "", gsub("T", " ", time)), tz = "GMT" )
    # myTime <- as.data.frame( as.POSIXct(time, origin = "1970-01-01", tz = "UTC") )
    max(myTime)
    f <- gsub("\\.", "_", f)
    tmp = cbind(myTime,myTOL)
    colnames(tmp) = c("dateTime",f)
    nc_close(nc)
    TOLall = rbind(TOLall, tmp)
  }
  
  
}
TOLall$yr = year(TOLall$dateTime)
# max(TOLall$dateTime)
#tmp = TOLall[ TOLall$yr == 2020, ]

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
    myTime = as.POSIXct( gsub(".000Z", "", gsub("T", " ", time)), tz = "GMT" )
    #myTime <- as.data.frame( as.POSIXct(time, origin = "1970-01-01", tz = "UTC") )
    
    tmp = cbind(myTime,myTOL)
    colnames(tmp) = c("dateTime","BB_20_24000")
    nc_close(nc)
    BBall = rbind(BBall, tmp)
  }
  
  
}

# READ DETECTIONS ####
shipsAll = NULL
siteD = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filesShip)), "_"), "[[", 3)
for (dd in 1:length(filesShip) ) { # dd = 9
  
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
    #max(tmp$End)
    nc_close(nc)
    shipsAll = rbind(shipsAll, cbind( tmp["Site"],tmp["Start"],tmp["End"],tmp["mins"],tmp["sourceType"]) )
    rm(tmp)
    
  }
}

# READ humpbacks ####
humpbacksAll = NULL
siteD = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filesDet)), "_"), "[[", 3)
for (dd in 1:length(filesDet) ) { # dd = 6
  
  siteOut = paste0( siteN, "_", siteD[dd])
  inFile = filesDet[dd]
  typ = sapply( strsplit(basename(inFile), "[.]"), "[[",2)
  
   if (typ == "csv") {
    tmp = read.csv(inFile)
    colnames(tmp) = c("ISOStartTime","Label" )
    # cat(basename(filesDet[dd]),  tmp$ISOStartTime[1], "\n")
  
    
    if (tmp$Label[1] != "No") {
      
      tmp$Site = siteOut
      
      
      if ( grepl( "/", tmp$ISOStartTime[1]) ){
        tmp$Start = as.POSIXct( tmp$ISOStartTime , format = "%m/%d/%Y", tz = "GMT" ) 
      }else if (  grepl( "-", tmp$ISOStartTime[1])  ) {
        tmp$Start = as.POSIXct( tmp$ISOStartTime , format = "%Y-%m-%d", tz = "GMT" ) 
      } else {
        tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
      }
      
      #tmp$mins = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "mins" )) )
      tmp$sourceType = detTypes[dd]
      
      humpbacksAll = rbind(humpbacksAll, cbind( tmp["Site"],tmp["Start"],tmp["sourceType"], paste0(siteN, "_", siteD[dd]))  )
      rm(tmp)
    }
  } else if (typ == "nc") {
    nc = nc_open(inFile)
    tmp = NULL
    start_time = as.data.frame( as.POSIXct(( ncvar_get(nc, "start_time")/1000   ), origin = "1970-01-01", tz = "UTC") )
    #end_time   = as.data.frame( as.POSIXct(( ncvar_get(nc, "end_time")/1000   ),   origin = "1970-01-01", tz = "UTC") )
    tmp$Start = start_time
    #tmp$End = end_time
    tmp = as.data.frame(tmp)
    colnames(tmp) = c("Start")
    #tmp$mins = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "mins" )) )
    tmp$Site = siteOut
    tmp$sourceType = detTypes[dd]
    #tmp$ship  = as.data.frame ( ncvar_get(nc, "ships_presence")   )
    nc_close(nc)
    humpbacksAll = rbind(humpbacksAll, cbind( tmp["Site"], tmp["Start"], tmp["sourceType"], paste0(siteN, "_", siteD[dd])) ) 
    rm(tmp)
    
  }
}
humpbacksAll$Start[ diff(humpbacksAll$Start) > 86400] # only between deployments

# COMBINE & OUTPUT ####
dataOut = merge(BBall,TOLall, by = "dateTime")
dataOut$Day = as.Date(dataOut$dateTime)
# median sound levels by day 
dataOutDay = aggregate( dataOut[, 2:32], by = list(Day = dataOut$Day), FUN = median)
colnames(dataOutDay)

## ships ####
# how many ship detection occur in each day
shipsAll$StartDay = as.Date( shipsAll$Start )
shipsAll$EndDay = as.Date( shipsAll$End )
dataOutDay$Ships = 0
for (bb in 1: nrow(shipsAll) ) { # bb = 20
  
  idx = which(dataOutDay$Day == as.Date(shipsAll$StartDay[bb])) #matching day
  dataOutDay$Ships[idx] =  dataOutDay$Ships[idx] + 1 # accumulate detections on a given day
  
  # add in days with end of detection on a different day
  if (shipsAll$EndDay [bb] - shipsAll$StartDay[bb] == 1) {
    dataOutDay$Ships[idx+1] =  dataOutDay$Ships[idx] + 1
  }
}
# unique(dataOutDay$Ships)

## humpbacks ####
# match each hour, if present in a day
matching_indices <- which(dataOutDay$Day %in% as.Date(humpbacksAll$Start))
dataOutDay$Humpback = 0
dataOutDay$Humpback[matching_indices] = 1
#unique(dataOutDay$Humpback)

## AIS ####
# match each hour, with daily values
AIS$Day = AIS$DATE
AIS_keep <-colnames(AIS)[3:11]
dataOutDay = merge(dataOutDay, AIS[AIS_keep], by = "Day")
# colnames(dataOutDay)

# DATA SUMMARIES ####
start_dates <- as.Date(c("2020-04-01", "2020-09-01", "2021-04-01", "2021-09-01","2019-04-01", "2019-09-01"))
end_dates <-   as.Date(c("2020-05-31", "2020-10-31", "2021-05-31", "2021-10-31","2019-05-31", "2019-10-31"))
timePeriod_s = c("SP20","FA20","SP21","FA21","SP19","FA19")
date_ranges <- data.frame(timePeriod = timePeriod_s, Start = start_dates, End = end_dates)
colnames(dataOutDay)

dataOutDay <- dataOutDay %>% select(-BB_20_24000 )
columns_of_interest = 2:31
sumOut = NULL
sumOut2 = NULL
for (ii in 1:nrow(date_ranges)){ # ii=1
  
  idx = which( dataOutDay$Day  >= date_ranges$Start[ii] & dataOutDay$Day  <= date_ranges$End[ii] )
  nSamples = length(idx) # number of days
  
  subset_data = dataOutDay[idx,]
  quants = apply(subset_data[, columns_of_interest], 2, quantile)
  quants50 = quants[3,]
  quants25 = quants[2,]
  quants75 = quants[4,]
  
  nHumps = sum( subset_data$Humpback >= 1 ) # days with humpbacks
  
  nShips = sum( subset_data$Ships >= 1 )   # days with ships
  avgShips = mean( subset_data$Ships[subset_data$Ships >= 1] )   # averge # detections on days with with ships
  
  #AIS summary
  #average unique vessels on days with vessels
  avgSUV =  mean( subset_data$LOA_S_UV[subset_data$LOA_S_UV >= 1] ) 
  avgMUV= mean( subset_data$LOA_M_UV[subset_data$LOA_M_UV >= 1] )
  avgLUV= mean( subset_data$LOA_L_UV[subset_data$LOA_L_UV >= 1] )
  subset_data$Total_UV <- rowSums(subset(subset_data, select = c(LOA_S_UV, LOA_M_UV, LOA_L_UV)), na.rm = TRUE)
  avgTUV =mean( subset_data$Total_UV[subset_data$Total_UV >= 1] )
  
  # average ophrs on days with vessels
  avgSHR = mean( subset_data$LOA_S_OPHRS[subset_data$LOA_S_OPHRS >= 1] ) 
  avgMHR = mean( subset_data$LOA_M_OPHRS[subset_data$LOA_M_OPHRS >= 1] )
  avgLHR = mean( subset_data$LOA_L_OPHRS[subset_data$LOA_L_OPHRS >= 1] )
  subset_data$Total_OPHRS <- rowSums(subset(subset_data, select = c(LOA_S_OPHRS, LOA_M_OPHRS, LOA_L_OPHRS)), na.rm = TRUE)
  avgTHR = mean( subset_data$Total_OPHRS[subset_data$Total_OPHRS >= 1] )

  sumOut = rbind(sumOut, c(siteN, date_ranges$timePeriod[ii],  nSamples, quants50, quants25, quants75,
                           nHumps, nShips, avgShips,
                           avgSUV, avgMUV, avgLUV, avgTUV,
                           avgSHR,avgMHR,avgLHR,avgTHR) )
  
  sumOut2 = rbind(sumOut2, c(siteN, date_ranges$timePeriod[ii],  nSamples, quants50,
                           nHumps, nShips, avgShips,
                           avgSUV, avgMUV, avgLUV, avgTUV,
                           avgSHR,avgMHR,avgLHR,avgTHR) )
  
  rm(nSamples, nHumps, nShips, quants50,quants25,quants75 )
}
sumOut = as.data.frame(sumOut)
lab50 = paste0( colnames(dataOut)[columns_of_interest], "_50th")
lab25 = paste0( colnames(dataOut)[columns_of_interest], "_25th")
lab75 = paste0( colnames(dataOut)[columns_of_interest], "_75th")
colnames(sumOut) = c("site", "TimePeriod", "Total_days",lab50, lab25, lab75,
                      "Hump_days", "Ship_days","avgShips_day",
                     "avgUV_S", "avgUV_M", "avgUV_L", "avgUV_all",
                     "avgHR_S","avgHR_M","avgHR_L","avgHR_all")
colnames(sumOut2) = c("site", "TimePeriod", "Total_days",lab50,
                     "Hump_days", "Ship_days","avgShips_day",
                     "avgUV_S", "avgUV_M", "avgUV_L", "avgUV_all",
                     "avgHR_S","avgHR_M","avgHR_L","avgHR_all")

melted_df50 <- melt(sumOut, id.vars = c("TimePeriod"), measure.vars = lab50)
melted_df25 <- melt(sumOut, id.vars = c("TimePeriod"), measure.vars = lab25)
melted_df75 <- melt(sumOut, id.vars = c("TimePeriod"), measure.vars = lab75)

# plot  percentile sound levels by time period
# as.numeric(as.character( sapply(strsplit(colnames(dataOutDay)[columns_of_interest], "_"), "[[", 2)))
melted_df50$freq = as.numeric(as.character( sapply(strsplit(as.character( melted_df50$variable) , "_"), "[[", 2)))
melted_df50$SPL50 = as.numeric(as.character(melted_df50$value ))

melted_df25$freq = as.numeric(as.character( sapply(strsplit(as.character( melted_df25$variable) , "_"), "[[", 2)))
melted_df25$SPL25 = as.numeric(as.character(melted_df25$value ))

melted_df75$freq = as.numeric(as.character( sapply(strsplit(as.character( melted_df75$variable) , "_"), "[[", 2)))
melted_df75$SPL75 = as.numeric(as.character(melted_df75$value ))

ggplot()+
  geom_line(data = melted_df50, aes(x = freq, y = SPL50, color = TimePeriod, group = TimePeriod),size = 2 ) +
  geom_line(data = melted_df25, aes(x = freq, y = SPL25, color = TimePeriod, group = TimePeriod) , linetype = "dotted") +
  geom_line(data = melted_df75, aes(x = freq, y = SPL75, color = TimePeriod, group = TimePeriod) , linetype = "dotted") +
  scale_x_log10() +
  scale_color_manual(values = c("FA19" = "black", "FA20" = "red","FA21" = "green","SP19" = "gray","SP20" = "lightcoral","SP21" = "lightgreen")) +
  theme_minimal() +
  labs(
    title = "",
    x = "Frequency (third-octave) Hz",
    y = "Median daily Sound Pressure Level for time period "
  )

#summary table

 # old way ####
## humpbacks ####
# match each hour, if present in a day
matching_indices <- which(dataOut$Day %in% as.Date(humpbacksAll$Start))
dataOut$Humpback = 0
dataOut$Humpback[matching_indices] = 1
# plot(dataOut$Humpback )
## ships ####
# how many ship detection occur in each hour
shipsAll$DurS = shipsAll$mins * 60
dataOut$Ships = 0
shipsAll$yr = year(shipsAll$Start) # unique( shipsAll$yr)
for (bb in 1: nrow(shipsAll) ) { # bb = tixd[1]
  
  # find all TOLs rows that fall with the detection period
  
  if ( shipsAll$mins[bb] <= 60 )  # if detection is less than a hour, need different logic... because only one row of data for that hour!
  {
    ttime = as.POSIXct(  format ( shipsAll$Start[bb], "%Y-%m-%d %H:00:00" )  , format = "%Y-%m-%d %H:%M:%S" , tz = "GMT")
    
    idx =  which( dataOut$dateTime  == ttime  )
    #dataOut$dateTime[idx]
  } else {
    
    idx = dataOut$dateTime >= shipsAll$Start[bb] & dataOut$dateTime <=  shipsAll$End[bb]
    
   
  }
  
  if (length(idx) > 0 ) 
  {
    dataOut$Ships[idx] =  dataOut$Ships[idx] + 1  #can be overlapping because all detections!
    #inHMDdata$Type[idx] =  paste( inHMDdata$Type[idx], detAll$Type[bb] , sep = ";") # keep track of types
  }
  
}
# unique(dataOut$Ships)
colnames(dataOut)
## AIS ####
# match each hour, with daily values
AIS$Day = AIS$DATE
dataOut = merge(dataOut, AIS, by = "Day")

# DATA SUMMARIES ####
start_dates <- as.Date(c("2020-04-01", "2020-09-01", "2021-04-01", "2021-09-01","2019-04-01", "2019-09-01"))
end_dates <-   as.Date(c("2020-05-31", "2020-10-31", "2021-05-31", "2021-10-31","2019-05-31", "2019-10-31"))
timePeriod_s = c("SP20","FA20","SP21","FA21","SP19","FA19")
date_ranges <- data.frame(timePeriod = timePeriod_s, Start = start_dates, End = end_dates)
columns_of_interest = 3:33

sumOut = NULL
for (ii in 1:nrow(date_ranges)){ # ii=1
  
  idx = which( dataOut$Day  >= date_ranges$Start[ii] & dataOut$Day  <= date_ranges$End[ii] )
  nSamples = length(idx)
  
  subset_data = dataOut[idx,]
  quants = apply(subset_data[, columns_of_interest], 2, quantile)
  nHumps = sum( subset_data$Humpback >= 1 )
  nShips = sum( subset_data$Ships >= 1 )
  quants50 = quants[3,]
  quants25 = quants[2,]
  quants75 = quants[4,]
  mean( subset_data$LOA_S_UV)
  mean( subset_data$LOA_S_UV)
  
  sumOut = rbind(sumOut, c(date_ranges$timePeriod[ii], nSamples, nHumps, nShips, quants50, quants25, quants75) )
  rm(nSamples, nHumps, nShips, quants50,quants25,quants75 )
}
sumOut = as.data.frame(sumOut)
lab50 = paste0( colnames(dataOut)[columns_of_interest], "_50th")
lab25 = paste0( colnames(dataOut)[columns_of_interest], "_25th")
lab75 = paste0( colnames(dataOut)[columns_of_interest], "_75th")
colnames(sumOut) = c("TimePeriod","Total_mins", "Hump_mins", "Ship_mins", lab50,lab25, lab75)

melted_df <- melt(sumOut, id.vars = c("TimePeriod"), measure.vars = c(colnames(dataOut)[columns_of_interest]))
ggplot(melted_df, aes(x = variable, y = as.numeric( as.character(value) ), color = TimePeriod))+
  geom_point()+
  theme_minimal()
