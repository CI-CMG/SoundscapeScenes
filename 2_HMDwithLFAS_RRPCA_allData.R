# De-noising methods for ambient samples 
# reads in all 1-min HMD data with AS labels
# selects only Ambient samples, and runs RRPCA
# saves out updated per site files

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
inFiles = list.files( inDir, pattern = "HMDdetLF", full.names = T, recursive = T)

inFiles = inFiles[!grepl("Spectra", inFiles)] #remove median spectra files
inFiles = inFiles[!grepl("previousVersion", inFiles)] #remove previous versions

fqr  = "LF"  #append this to output names
site = "All"
DC = Sys.Date()
dirOut = "F:\\SanctSound\\analysis\\combineFiles_AcousticScene"

RRPCAsumOUT = NULL # summary of percentiles for each site

# LOOP through sites ####
for (f in 1: (length(inFiles)-1) )  {
  load( inFiles[f])
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 2) #site name
  HMDdet$Site = st
  cat("Processing ", basename( inFiles[f]) ,"\n")
  
  idNA = ( which(is.na(HMDdet))) # check for NAs, as.data.frame( colnames( Ambient )[1:10] )
  idx  = grep("^X", colnames(HMDdet))
  hix  = as.numeric( gsub("X","", names(HMDdet)[idx]) )
  Nv   =  HMDdet[, idx]  #dB values
  NvP  = 10^(Nv/20)     #pressure values
  nvDate = HMDdet$dateTime
  
  ## truncate to 100-1kHz ####
  fe =  which(hix == 1001.2)
  NvPt = NvP[,1:fe]
  Nv   = Nv[ ,1:fe]
  hix  = hix[1:fe]
  
  ## RRPCA ####
  # Robust principal components analysis separates a matrix into a low-rank plus sparse component
  #a method for the robust separation of a rectangular (m, n) matrix A into a low-rank component L and a sparse component S
  # input = ( NvP ) 
  lamd = max(NvPt)^-0.5 #default settings
  nvpcaTOL = rrpca(NvPt)
  sampleHours = nrow(NvP)
  save(nvpcaTOL, file = paste0(dirOut, "\\RRPCA_HMD_allSites_", st, "_", DC, ".Rda") )
  
  ## (option to load rrpca results here) ####
  
  ## RRPCA results ####
  #low rank
  Lr = as.data.frame(nvpcaTOL$L) 
  colnames(Lr) = hix
  LrDB = 10*log10( Lr^2 )  #CHECK: min(LrDB$`63`), no negative values, just values without transients
  colnames(LrDB) = hix
  #sparse matrix
  Sp = as.data.frame(nvpcaTOL$S) 
  colnames(Sp) = hix
  SpDB = 10*log10( (Sp)^2 ) # negative and zero values-- does not make sense to convert back to dB
  colnames(SpDB) = hix
  
  ## RRPCA thresholds ####
  # sum of difference across frequencies for each minute
  LRdiff = as.data.frame ( rowSums( abs ( (LrDB - Nv) ) ) )
  colnames(LRdiff) = 'LRdiff'
  LRfq   = as.data.frame ( as.numeric ( colnames(LrDB) [apply(LrDB, 1, (which.max) )] ) )
  colnames(LRfq) = 'LRfq'
  # sum of sparce across frequencies for each minute
  SPsum = as.data.frame  ( rowSums( abs ( Sp ) ) )
  colnames(SPsum) = 'SPsum'
  
  ## label files ####
  HMDdet$LowRanK = as.numeric( as.character(LRdiff$LRdiff ) )
  HMDdet$Sparce  = as.numeric( as.character(SPsum$SPsum  ) )
  HMDdet$LRfq = LRfq$LRfq
  
  ## percentile for thresholds ####
  RRPCAsum = as.data.frame ( rbind ( quantile(HMDdet$LowRanK, na.rm = T),
                                     quantile(HMDdet$LRfq, na.rm = T),
                                     quantile(HMDdet$Sparce, na.rm = T) ) )
  RRPCAsum$Site = st
  RRPCAsum$RRPCAmetric = c("LR-sum","LR-freq","SP-sum")
  RRPCAsumOUT = rbind(RRPCAsumOUT, RRPCAsum )
  
  ## write out new HMD+ ####
  save(HMDdet, file = paste0(dirOut, "\\HMDdets_RpcaSite_", st, "_", DC, ".Rda") )
 
} # end site loop

save(RRPCAsum, file = paste0(dirOut, "\\RRPCAsum_bySite", "_", DC, ".Rda") )
