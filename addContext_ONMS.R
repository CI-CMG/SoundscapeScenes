#ADD context metadata to hourly TOLs
# wind (PAMscapes), AIS (Jeff Adams), ONMS metadata
# works for each monitoring site
# check for files that are already processed...

rm(list=ls()) 
library(PAMscapes)

DC = Sys.Date()

site = "mb02"
outputDir = paste0( "F:/ONMS/", site,"/")
outDir = paste0(outputDir, tolower(site),"/" )

#ONMS Metadata 
metaFile = paste0("F:\\CODE\\GitHub\\SoundscapeScenes\\NCEI summary\\ONMSSound_IndicatorCategories.xlsx")

# LOAD SPL DATA - HOURLY TOLs   
inFile = list.files(outputDir, pattern = paste0("data_",site,"_HourlySPL"), full.names = T)
file_info <- file.info(inFile)
load( inFile[which.max(file_info$ctime)] )
st = as.Date( min(aData$UTC) )
ed = as.Date( max(aData$UTC) )
udays = length( unique(as.Date(aData$UTC)) )
cat("Input Data - ", site, " has ", udays, " unique days")
# CHECK FOR PREVIOUS DATA (underconstruction)
#exFile = list.files(outputDir, pattern = paste0("data_",site,"_Context"), full.names = T)
#truncate to just the new data in aData and processes, then recombine



