# check site HMD data- Acoustic scene analysis

rm(list=ls()) 
library(lubridate)

ftxt   = "HMDcheck"
dirOut = "F:\\SanctSound\\analysis\\combineFiles_AcousticScene"

inFiles = list.files(dirOut, pattern=ftxt, full.names = T)
outFile = NULL

for (f in 1:length(inFiles)) {
  tmp = read.csv(inFiles[f])
  tmp$Date = ymd( sapply(strsplit(basename( tmp$FileName), "_"), "[[", 4) ) 
  
  site = ( sapply(strsplit(basename( tmp$FileName), "_"), "[[", 1) ) 
  deployment   = ( sapply(strsplit(basename( tmp$FileName), "_"), "[[", 2) ) 
  
  uniqueDays   = length(unique(tmp$Date))   # unique days in the dataset
  remv = sum( tmp$secondsInFile_all )- sum( tmp$secondsInFile )
  # remv  =  abs( sum( tmp$MinsRemoved ) )    # minutes removed across all files, because not 00 seconds
  totalMinutes = sum(tmp$secondsInFile)/60  # total minutes sampled
  
  #How much data are missing in total?
  # seconds in file - expected seconds across all days
  pData =  abs( sum( tmp$secondsInFile ) - uniqueDays*1440 )/ (uniqueDays*1440) *100
  
  #How much data missing because 00 error?
  # seconds removed - total seconds in file
  pData2 =  (abs( sum( tmp$MinsRemoved ) ) / sum(tmp$secondsInFile) )*100
  
 

  startDate =  min(tmp$Date)
  endDate =  max(tmp$Date)
  outFile = rbind(outFile, c(site[1], deployment[1], uniqueDays, totalMinutes,
                             as.character(startDate), as.character(endDate), remv, pData, pData2) )
  
  
}
outFile = as.data.frame(outFile)


sum(as.numeric( as.character( outFile$V3) ))
