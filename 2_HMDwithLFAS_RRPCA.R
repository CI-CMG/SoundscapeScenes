# De-noising methods for ambient samples 
# reads in all 1-min HMD data with AS labels
# selects only Ambient samples, and runs RRPCA

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)
library(tidyverse)
library(rsvd)

#GET DATA ####
inDir = (  "F:\\SanctSound\\analysis\\combineFiles_AcousticScene" )
inFiles = list.files( inDir, pattern = "HMDdetLF", full.names = T)
pltf = 0
fqr = "LF"  #append this to output names
site = "All"

#PREP DATA ####
Ambient = NULL

for (f in 1: length(inFiles)) { # f = 6 for testing
  
  load( inFiles[f])
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 3) #site name
  HMDdet$Site = st
  
  Ambient = rbind (Ambient, HMDdet[HMDdet$Category == "Ambient",] )
  
  
}

idNA = which(is.na(Ambient)) # check for NAs
# as.data.frame( colnames( Ambient )[1:10] )

idx  = grep("^X", colnames(Ambient))
hix  = as.numeric( gsub("X","", names(Ambient)[idx]) )
Nv   =  Ambient[, idx]  #dB values
NvP  = 10^(Nv/20)     #pressure values
nvDate = Ambient$dateTime


#ANALYSIS ####
# Robust principal components analysis separates a matrix into a low-rank plus sparse component
#a method for the robust separation of a rectangular (m, n) matrix A into a low-rank component L and a sparse component S
input = ( NvP ) 
lamd = max(input)^-0.5 #default settings
nvpcaTOL = rrpca(input)
sampleHours = nrow(input)

#SUMMARIZE OUTPUT ####
#input data 
Am = as.data.frame(input)   
#low rank
Lr = as.data.frame(nvpcaTOL$L) 
colnames(Lr) = hix
LrDB = 10*log10( Lr^2 )  #CHECK: min(LrDB$`63`), no negative values, just values without transients
colnames(LrDB) = hix
LrDB =as.matrix(LrDB)
#sparse matrix
Sp = as.data.frame(nvpcaTOL$S) 
colnames(Sp) = hix
SpDB = 10*log10( (Sp)^2 ) # negative and zero values-- does not make sense to convert back to dB
colnames(SpDB) = hix

# VISULAZE DATA ####
#LOW RANK 
#spectra for each hour-- lots of data, so truncate to month of interest
NvMlr   = reshape :: melt(t(LrDB)  )
tLabel  = paste0( site, "-", as.character(min(nv$DateF)), "-", as.character( max(nv$DateF) ), " (hours = ", length(nv$DateF), ")")
uhrs = unique( NvMlr$X2 )
dateInfo = as.data.frame( cbind((uhrs), (as.character(nvDate) )) )
dateInfo$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", dateInfo$V2)), tz = "GMT" )
dateInfo$yr  = year(dateInfo$DateF )
dateInfo$mth = month(dateInfo$DateF )
dateInfo$hr  = hour(dateInfo$DateF )
dateInfo$jday  = yday(dateInfo$DateF )
dateInfo$wday  = weekdays(dateInfo$DateF )
dateInfo$day  = as.Date(dateInfo$DateF )
idx    = uhrs[which(dateInfo$mth  == 4)] #only select April days

#original matrix
NvMO   = reshape :: melt(t(Nv)  )
NvMOt  = NvMlr[NvMO$X2 %in% idx,]
pO = ggplot(NvMOt, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous(trans = "log10")+ 
  labs(title = paste0(site, ": Original (plot for ", smth, ") \n Total Hours : ",sampleHours ))+
  xlab("Frequency (HMD)") + ylab("1-min PSD")+
  ylim(c(60,114))+
  theme_minimal()

# RE-LABEL HMD FILES ####
# loop through sites
usite = unique(Ambient$Site)
