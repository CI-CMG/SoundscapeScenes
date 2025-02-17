# ONMS- how does your sanctury compare?

#processes data from SantSound
rm(list=ls())
library(PAMscapes)

inDir = "F:\\SanctSound" # SANCTSOUND
sites = c("MB01","MB02") # "mb01"

#NOTE- might need to change these in some of the files 31_5 to 31.5 and UTC with : not _
inFiles  = list.files(path = inDir, pattern = "TOL_1h", full.names = T, recursive = T)
inFiles = inFiles[!grepl("/analysis/", inFiles)] 
inFiles = inFiles[!grepl("1h.nc", inFiles)] 
#inFiles = unique (grep(paste(sites,collapse="|"), inFiles, value=TRUE))

dataTOL = NULL
if (length(inFiles) > 0 ) {
  for (ii in 1:length(inFiles)) { # ii = 9
    
    tmpFile = inFiles[ii]
    typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
    site = sapply( strsplit(basename(tmpFile), "_"), "[[",2)
    
   # read.csv(tmpFile) #yyyy.mm.ddTHH.MM.SSZ NOT yyyy_mm_ddTHH_MM_SSZ
    
    tmp = as.data.frame( loadSoundscapeData( inFiles[ii], extension = typ) )
    tmp = cbind(site,tmp)
    
    ck = which( names(tmp) == "TOL_31_5" )
    if( length(ck)>0 ) { names(tmp)[ck] ="TOL_31.5" }
    
    cat( inFiles[ii], "Start = ", as.character( as.Date( min(tmp$UTC) ) ),"\n")
    dataTOL = rbind(dataTOL, (tmp) )

  }

  dataTOL$yr   = year(dataTOL$UTC)
  dataTOL$mth  = month(dataTOL$UTC)
  
}