# De-noising methods HMD files

# WORKS one site at a time
#AU_CH01

# INPUT
#output of B_HmdLabels (Rdat)

# OUTPUT
#RRPCA model results (Rdat)
#RRPCA thresholds with date column and 3 RRPCA values (Rdat)

# NEXT

rm(list=ls()) 

# LIBRARIES ####
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)
library(tidyverse)
library(rsvd)

# GET DATA ####
siteN = "AU_CH01"
filepat = "_HmdLabels_LF_"
gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
dirIn =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS\\", siteN )
inFiles = list.files( dirIn, pattern = filepat, recursive = F, full.names = T )
load( inFiles )
dirOut = dirIn

# SET UP PARAMS ####
RRPCAsumOUT = NULL # summary of percentiles for each site
fqr  = "LF"  
DC = Sys.Date()
rrpca1 = -0.5 #default settings
LB = "LF" #what label do you want to indicate on the ouutput file, LF = low frequency

# DATA PREP ####
HmdDets$season <- factor(HmdDets$season, ordered = TRUE, levels = c("form", "ice", "break", "open"))
# unique( HmdDets$season)
idNA = ( which(is.na(HmdDets)))
numeric_columns <- grep("^\\d", names( HmdDets) )  
hix = names( HmdDets)[numeric_columns]
Nv   =  HmdDets[, numeric_columns]  #dB values
NvP  = 10^(Nv/20)     #pressure values
nvDate = HmdDets$dateTime
sampleHours = nrow(NvP)

## RRPCA ####
# Robust principal components analysis separates a matrix into a low-rank plus sparse component
#a method for the robust separation of a rectangular (m, n) matrix A into a low-rank component L 
# and a sparse component S
lamd = max(NvP)^(-rrpca1) #default settings
nvpcaTOL = rrpca(NvP)
save(nvpcaTOL, file = paste0(dirOut, "\\", siteN,  "_RrpcaResults_", DC, ".Rda") )
# load( paste0(dirOut, "\\", "AU_CH01_RrpcaResults_2024-06-24.Rda") )

## results ####
#low rank
Lr = as.data.frame(nvpcaTOL$L) 
colnames(Lr) = hix
LrDB = 10*log10( Lr^2 )  #CHECK: median(LrDB$'100'), no negative values, just values without transients
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
min(LRdiff$LRdiff)

# which frequency had the max LF diff
LRfq   = as.data.frame ( as.numeric ( colnames(LrDB) [apply(LrDB, 1, (which.max) )] ) )
colnames(LRfq) = 'LRfq'
median( LRfq$LRfq )

# sum of sparce across frequencies for each minute
SPsum = as.data.frame  ( rowSums( abs ( Sp ) ) )
colnames(SPsum) = 'SPsum'
min(SPsum$SPsum)

## LABEL HMD files ####
HmdDets$LowRanK = as.numeric( as.character(LRdiff$LRdiff ) )
HmdDets$Sparce  = as.numeric( as.character(SPsum$SPsum  ) )
HmdDets$LRfq = LRfq$LRfq

### scatter by season, category & RRPCA values ####
ggplot(data = HmdDets, aes(x = LowRanK, Sparce, color = LRfq)) +
  geom_point(alpha = .1) + 
  scale_x_log10() +   scale_y_log10() +
  facet_wrap(~season) +
  ggtitle("How soundscape conditions group by season")+
  theme_minimal()+
  theme(text = element_text(size = 16) )

## THRESHOLDS ####
# lowrank = sum difference across frequencies and 15th percentile of all values
# 15% because wind speed might be driving the super high values
thrLR = round( quantile (HmdDets$LowRanK, .15, na.rm = T )) 
# sparce = sum of abs values across frequencies and 75 percentile
thrSP = round( quantile (HmdDets$Sparce, .75, na.rm = T )) 
#  which minutes fall below these thresholds?
indx = ( which( HmdDets$LowRanK < thrLR & HmdDets$Sparce < thrSP  ) )
# Percent of samples Background: 
(nrow(HmdDets)- length(indx) )/ nrow(HmdDets) 
HmdDets$RRPCA = "Background"
HmdDets$RRPCA[indx] = "Transient"

# WRITE out new HMD+ ####
category_columns = which(colnames(HmdDets) == "LowRanK" ) 
category_columns1 = which(colnames(HmdDets) == "Sparce" ) 
category_columns2 = which(colnames(HmdDets) == "LRfq" ) 
season_columns = which(colnames(HmdDets) == "season" ) 

outRrpca = as.data.frame( HmdDets[, c(1, season_columns, 
                                      category_columns, category_columns1, category_columns2,
                                      which(colnames(HmdDets) == "RRPCA") ) ] )
save(outRrpca, file = paste0(dirIn, "\\", siteN,  "_HmdCondition_", LB ,"_",DC) ) 

# RESULTS ####
RRPCAsum = as.data.frame ( rbind ( quantile(HmdDets$LowRanK, na.rm = T),
                                   quantile(HmdDets$LRfq, na.rm = T),
                                   quantile(HmdDets$Sparce, na.rm = T) ) )
RRPCAsum$Site = siteN
RRPCAsum$RRPCAmetric = c("LR-sum","LR-freq","SP-sum")
RRPCAsumOUT = rbind(RRPCAsumOUT, RRPCAsum )
#save(RRPCAsum, file = paste0(dirOut, "\\RRPCAsum_bySite", "_", DC, ".Rda") )
