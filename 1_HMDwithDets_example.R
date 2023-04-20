# Purpose: label and Integrate 1-min hybrid-milli-decade (HMD) data with event based detections
# Event detections = detection periods with start and end time

rm(list=ls()) 

library(data.table)

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

# HMD DATA ####
## (option 1) HMD files (csv format, output of Manta) ####
inDir = choose.dir(default = "F:\\SanctSound\\AcousticScene_1min" , caption = "directory with HMD csv files" ) # HI04_02
inFiles= list.files(inDir, pattern = ".csv", full.names = T)
ii = 2
inFile = inFiles[ii]
inHMDcsv = read.csv(inFile)
basename( inFile )
st = sapply(strsplit(basename( inFile ), "_"), "[[", 1) #site name
dy = as.Date ( gsub (".csv","", sapply(strsplit(basename( inFile ), "_"), "[[", 4) ), format="%Y%m%d" )
cat("Processing... ",st," on " ,as.character( dy), "[", ii, " of ", length(inFiles),"]" )
colnames(inHMDcsv)[1] = "dateTime"
inHMDcsv$dateTime = as.POSIXct(   inHMDcsv$dateTime, format = "%d-%b-%Y %H:%M:%S" , tz = "GMT" ) # Date format: format the date (? will netCDF files be the same?)
fq = as.numeric(as.character( gsub("X","", colnames(inHMDcsv[3:ncol(inHMDcsv)] )) ) ) # Frequency range: truncate to 100-2000 Hz
st = which(fq == 100)+2      #  colnames(inHMDcsv)[st]
ed = which(fq == 1997.6)+2   #  colnames(inHMDcsv)[ed]
inHMDdata = as.data.frame( inHMDcsv[, c(1, st:ed )] )
fq = as.numeric(as.character( gsub("X","", colnames(inHMDdata[2:ncol(inHMDdata)] )) ) ) # Frequency range: truncate to 100-2000 Hz
rm(ed,st,inHMDcsv,ii,dy)
#OUTPUT:  inHMDdata

## (option 2) PSD files (csv format, output of Triton Soundscape Metrics) #### 
#run createDailyPSD_SoundscapeMetrics.R first to break up HUGE output of Trition
inDir   = choose.dir(default = "F:\\SanctSound\\AcousticScene_1min" , caption = "directory with PSD csv files" ) # CI03_04
inFiles = list.files(inDir, pattern = "PSD_", full.names = T)
ii = 1
inFile = inFiles[ii] 

inHMDcsv$TimeStamp = as.POSIXct( gsub(".000Z", "", gsub("T", " ", inHMDcsv$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" )
fq = as.numeric(as.character( gsub("PSD_","", colnames(inData[2:ncol(inData)] )) ) ) # Frequency range: truncate to 100-2000 Hz
st = which(fq == 100) +1 
ed = which(fq == 2000) +1 
inDataT = as.data.frame( inData[,c(1,st:ed)] )
rm(ed,st,inFile,inData)

## (option 3) HMD files (netCDF format, output of Manta) #### 
inDir = choose.dir(default = "F:\\SanctSound\\AcousticScene_1min" , caption = "directory with HMD nc files" ) # HI04_02
inFiles= list.files(inDir, pattern = ".nc", full.names = T)
ii = 2
inFile = inFiles[ii]
inHMDcsv = read.csv(inFile)

# DETECTIONS #### 
inDir= choose.dir() #all detections
list.dirs(inDir)



# PLOT OF LABELED DATA #### 
 