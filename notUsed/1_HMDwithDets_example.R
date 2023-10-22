# Purpose: label and Integrate 1-min hybrid-milli-decade (HMD) data with event based detections
# Event detections = detection periods with start and end time

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

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

# DIRECTORIES ####
dirSPL = "I:\\SanctSound_AcousticScene" # ?? create loop through all manta directories ??
dirDets = "F:\\SanctSound\\AcousticScene_subset\\detections"
pltf = 0 # change to 1 if you want to plot daily 1-min spectra

# MANTA HMD DATA ####
## (option 1) HMD files (csv format, output of Manta)
# runs one site at a time...
inDir  = choose.dir(default = dirSPL , caption = "Site directory with HMD csv files" ) # GR01_01
inHMD =  list.files(path = inDir, pattern = "MinRes.csv", full.names = T,recursive = T)
st = sapply(strsplit(basename( inHMD [1] ), "_"), "[[", 1) #site name
dpl = sapply(strsplit(basename( inHMD[1] ), "_"), "[[", 2) # deployment name

## ALL DETECTIONS ####
detFiles = list.files(path = dirDets, pattern = paste(st,dpl,sep="_"), full.names = T, recursive = T)
detFiles = detFiles[!grepl("1d", detFiles)] #remove 1 day files
detFiles = detFiles[!grepl("1h", detFiles)] #remove 1 hour files
detFiles = detFiles[!grepl("metadata", detFiles)] #remove metadata
#specific files of interest... !! site dependent!!
## VESSEL detections ####
detShips = detFiles[grepl("*hips.csv", detFiles)]    
inFilesVD = basename(detShips)
VD = read.csv(detShips)
colnames(VD) = c("ISOStartTime","ISOEndTime","Label" )
VD$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOStartTime)), tz = "GMT" )
VD$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOEndTime)),   tz = "GMT" )
VD$Site = st
VD$Dep  = dpl
VD$Yr  = year(VD$Start )
VD$Mth = month(VD$Start )
VD$DurS = as.numeric(as.character( difftime(VD$End, VD$Start, units = "secs" )) )
VD$DurH = VD$DurS/3600 
VD = VD[VD$Label == "ship", ] # head(VD)
## FISH detections ####  
# !! make more flexible? inDet = choose.file(), rename, and formate columns !!
detFish  = detFiles[grepl("FishChoruses", detFiles)]  
FD = readxl::read_excel(detFish)
FD$Start = force_tz( as.POSIXct( FD$Start_Time_UTC, tz = "GMT" ), tzone = "GMT" )
FD$End   = force_tz(as.POSIXct( FD$End_Time_UTC,   tz = "GMT"  ), tzone = "GMT" )
FD$Site = st
FD$Dep  = dpl
FD$Yr   = year(FD$Start)
FD$Mth  = month(FD$Start)
FD$DurS = as.numeric(as.character( difftime(FD$End, FD$Start, units = "secs" )) )
FD$DurH = FD$DurS/3600 
FD = as.data.frame(FD) #head(FD)

# !! start daily loop !! ####
HMDdet = NULL

for (ii in 1:length(inHMD)){ #length(inHMD)
  
  #read in daily files
  inFile = inHMD[ii]
  inHMDcsv = read.csv(inFile) # basename( inFile )
  ck = 1440-dim(inHMDcsv)[1]
  dy = as.Date ( gsub (".csv","", sapply(strsplit(basename( inFile ), "_"), "[[", 4) ), format="%Y%m%d" )
  
  #truncate HMD data
  colnames(inHMDcsv)[1] = "dateTime"
  inHMDcsv$dateTime = as.POSIXct(   inHMDcsv$dateTime, format = "%d-%b-%Y %H:%M:%S" , tz = "GMT" ) # Date format: format the date (? will netCDF files be the same?)
  fq = as.numeric(as.character( gsub("X","", colnames(inHMDcsv[3:ncol(inHMDcsv)] )) ) ) # Frequency range: truncate to 100-2000 Hz
  str = which(fq == 100)+2      #  colnames(inHMDcsv)[st]
  ed = which(fq == 1997.6)+2   #  colnames(inHMDcsv)[ed]
  inHMDdata = as.data.frame( inHMDcsv[, c(1, str:ed )] )
  fq = as.numeric(as.character( gsub("X","", colnames(inHMDdata[2:ncol(inHMDdata)] )) ) ) # Frequency range: truncate to 100-2000 Hz
  rm(ed,str, inHMDcsv)
  
  # plots spectra
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
  
  #merge HMD + detections
  inHMDdata$VD = 0
  for (dd in 1:nrow(VD) ){
    # find all TOLs rows that fall with the detection period
    idx =  which( inHMDdata$dateTime  >= VD$Start[dd] &  inHMDdata$dateTime + 60 < VD$End[dd] )
    #  Check : length(idx) -  (VD$End[dd] - VD$Start[dd])
    inHMDdata$VD[idx] = dd  # label TOL rows with vessel detection number- should not be overlap!
  }
  # unique(inHMDdata$VD) # not all detections labeled because only 1-day, not whole deployment
  
  inHMDdata$FD = 0
  inHMDdata$Chorus_Type = "none"
  # not all detections as labeled because only 1-day, not whole deployment
  for (dd in 1:nrow(FD) ){
    
    # find all TOLs rows that fall with the detection period
    idx =  which( inHMDdata$dateTime  >= FD$Start[dd] &  inHMDdata$dateTime + 60 < FD$End[dd] )
    #  Check : length(idx) -  (VD$End[dd] - VD$Start[dd])
    # FD$Chorus_Type[dd]
    if(length(idx) > 0 ){
      cat(dd, "\n")
      inHMDdata$FD[idx]          =  inHMDdata$FD[idx] + 1  # with chorus presence- can be overlapping
      inHMDdata$Chorus_Type[idx] =  paste( inHMDdata$Chorus_Type[idx], FD$Chorus_Type[dd] , sep = ";")
      
    }
    
  }
  
  # unique(inHMDdata$FD) 
  HMDdet = rbind(HMDdet, inHMDdata) 
  
  cat("Processing... ",st," on " ,as.character( dy), "[", ii, " of ", length(inHMD)," days ]", ck, " minutes missing","\n" )
  cat("Vessel Detections: ", length( unique( inHMDdata$VD ) ) ,"\n" )
  cat("Fish Detections: ",   length( unique( inHMDdata$FD ) ) ,"\n" )
  
} ## !! end a daily loop !! ####

# Detections added summary
unique( HMDdet$VD )
unique( HMDdet$FD )
unique( HMDdet$Chorus_Type )


# plot labeled spectra-- select specific data
HMDdet$Day = as.Date( HMDdet$dateTime )
tmpD = HMDdet[ HMDdet$Day == "2019-04-28", ]
ed = ncol(tmpD) -4
if (pltf == 1) {
  medSPLm = reshape::melt (tmpD, id.vars = c("dateTime", "Chorus_Type"),  measure.vars = colnames(tmpD)[ 2 : ed ] )
  colnames( medSPLm)  = c("date", "Det", "Fq", "SPL")
  medSPLm$Fq = as.numeric(as.character( gsub("X","", medSPLm$Fq ) ) ) #head(medSPLm)
  
  ggplot(medSPLm, aes(x = Fq, y=SPL, group = date ) ) +
    geom_line(alpha = .2 ) + 
    scale_x_log10() +
    ylab("1-min HMD")+ xlab("Frequency (Hz)")+
    facet_wrap(~Det)+
    theme_minimal() +
    ggtitle(paste0( st, " on ", dy) ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

#ACOUSTIC SCENE LABELS ####
unique(HMDdet$VD)
unique(HMDdet$Chorus_Type)
HMDdet$Category = "Ambient"
HMDdet$Category[HMDdet$VD > 0  & HMDdet$Chorus_Type != "none"] = "Bio+Anthro"
HMDdet$Category[HMDdet$VD > 0  & HMDdet$Chorus_Type == "none"] = "Anthro"
HMDdet$Category[HMDdet$VD == 0 & HMDdet$Chorus_Type != "none"] = "Bio"
unique(HMDdet$Category)


# SITE GRAPHIC of Acoustic Scenes ####
as.data.frame( HMDdet %>% group_by(Category) %>% tally() )
outDir = "G:\\My Drive\\ActiveProjects\\SANCTSOUND\\combineFiles_AcousticScene\\"
save(HMDdet, file = paste0(outDir, "HMDdet_", st,"_", dpl, ".Rda"))


# START HERE: 
# get start and end of different , so segment works for plotting ####
ggplot(outputVD, aes(x=Start, xend=End, y=Category, yend=Category, color=`TOL_125 max`)) +
  geom_segment()+
  theme_bw()+ 
  scale_y_discrete(limits=rev)+ 
  geom_segment(size=12) +
  xlab("")+  ylab("")+ ggtitle("Summary of Vessel Detection Periods") +
  labs(caption = (paste0("samples in each category: A=", tal$n[1]," | B=", tal$n[2]," | C=", tal$n[3]," | D=", tal$n[4] )))+
  scale_color_gradientn(colours = viridis(10))+
  theme(  axis.text.y = element_text(size = 14, colour="black"),
          axis.text.x=element_text(size = 14, colour="black"),
          plot.caption = element_text(size = 14) )

# NOT WORKING, yet ####
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
