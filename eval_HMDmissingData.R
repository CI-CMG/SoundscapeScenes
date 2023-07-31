# check site HMD data- Acoustic scene analysis

rm(list=ls()) 
library(lubridate)

ftxt = "HMDcheck"
dirOut = "F:\\SanctSound\\analysis\\combineFiles_AcousticScene"

inFiles = list.files(dirOut, pattern=ftxt, full.names = T)
outFile = NULL

for (f in 1:length(inFiles)) {
  tmp = read.csv(inFiles[f])
  tmp$Date = ymd( sapply(strsplit(basename( tmp$FileName), "_"), "[[", 4) ) 
  
  site = ( sapply(strsplit(basename( tmp$FileName), "_"), "[[", 1) ) 
  deployment = ( sapply(strsplit(basename( tmp$FileName), "_"), "[[", 2) ) 
  uniqueDays = length(unique(tmp$Date))
  totalMinutes = sum(tmp$secondsInFile)/60
  startDate =  min(tmp$Date)
  endDate =  max(tmp$Date)
  outFile = rbind(outFile, c(site[1], deployment[1], uniqueDays, totalMinutes,
                             as.character(startDate), as.character(endDate) ) )
  
  
}
outFile = as.data.frame(outFile)
sum(as.numeric( as.character( outFile$V3) ))
