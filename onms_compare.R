# How does your sanctuary compare?
# based on sanctsound data

rm(list=ls()) 
library(xlsx)
library(PAMscapes)
library(lubridate)

dataDir = "F:\\SanctSound\\"
outDir  = "F:\\CODE\\GitHub\\SoundscapeScenes\\NCEI summary\\"

# LOAD ONMS Metadata ####
metaFile = paste0(outDir,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( read.xlsx(metaFile, sheetIndex = 1) )
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
sites = lookup[lookup[,7] == "shelf" ,] #oceanographic setting
siteInterest = sites[,5]
# McKenna et al 2021 paper, nearshore shelf sites
#siteInterest = c("hi01", "sb01", "gr01","fk02","oc01","mb01","ci01","oc02","mb02") 


## SanctSound FILES - ERDAP ####
# NOTE - might need to change these in some of the files 31_5 to 31.5 and UTC with : not _
inFiles = list.files(path = dataDir, pattern = "TOL_1h", full.names = T, recursive = T)
filesTOL = inFiles[grepl("TOL_1h", inFiles)] 
inFiles = filesTOL[!grepl("/analysis/", filesTOL)] 
inFiles = inFiles[!grepl("1h.nc", inFiles)] 
inFiles = inFiles[!grepl("1h.nc", inFiles)] 
inFiles = inFiles[sapply(inFiles, function(x) any(grepl(paste(toupper( siteInterest ), collapse = "|"), x)))]

sData = NULL
for (ii in 1:length(inFiles)) { # ii = 1
  tmpFile = inFiles[ii]
  typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
  tmp = loadSoundscapeData( inFiles[ii], extension = typ)
  tmp$yr = year(tmp$UTC)
  tmp = tmp[tmp$yr == 2019, ]
  cat( inFiles[ii], "Start = ", as.character( as.Date( min(tmp$UTC) ) ),"\n")
  sData = rbind(sData, tmp)
}

sData$site = tolower(site)
sData$yr   = year(sData$UTC)
sData$mth  = month(sData$UTC)
inFilesS = inFiles

