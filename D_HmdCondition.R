# De-noising methods HMD files

# WORKS one site at a time
#AU_CH01, SB03, and caseStudy #2

# INPUT
#output of B_HmdLabels.R (Rdat) OR A_formatHmd.R

# OUTPUT
#RRPCA model results (Rdat)
#RRPCA thresholds with date column, 3 RRPCA values (Rdat), plots

rm(list=ls()) 

# LIBRARIES ####
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)
library(tidyverse)
library(rsvd)
library(rlang)

# GET DATA ####
siteN = "AU_CH01"
siteN = "SB03"
siteN = "CaseStudy2"

# DATA PREP ####
if (siteN == "AU_CH01"){
  filepat = "_HmdLabels_LF_"
  voi= "season" # "mth" variable of interst
  gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
  dirIn =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS\\", siteN )
  inFiles = list.files( dirIn, pattern = filepat, recursive = F, full.names = T )
  load( inFiles )
 
  HmdDets$season <- factor(HmdDets$season, ordered = TRUE, levels = c("form", "ice", "break", "open"))
}else if (siteN == "SB03"){
  filepat = "_Hmd_LF_"# "_HmdLabels_LF_"
  voi= "mth" #variable of interst
  gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
  dirIn =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS\\", siteN )
  inFiles = list.files( dirIn, pattern = filepat, recursive = F, full.names = T )
  load( inFiles )
  
}else if (siteN == "CaseStudy2"){
  filepat = "_Hmd_LF_"# "_HmdLabels_LF_"
  voi= "site" #variable of interst
  gdrive = "F:\\SoundCoop\\hmd_downloadedGCP\\"
  dirIn =  paste0( gdrive, siteN )
  inFiles = list.files( dirIn, pattern = filepat, recursive = F, full.names = T )
  dirOut = dirIn
}


if ((siteN == "CaseStudy2")){
  
  for (ii in 1:length(inFiles)) {
    # site name
    split_string <- strsplit(basename( inFiles[ii]), "_")[[1]]
    site <- split_string[1]
    
    # load & format data
    load( inFiles[ii] )
    HmdTrim$mth = month(HmdTrim$dateTime)
    HmdDets = HmdTrim
    idNA = ( which(is.na(HmdDets)))
    
    #truncate frequency to 100-1000 Hz
    stp = which( names( HmdDets) ==  "1001.2")
    HmdDets2 = HmdDets[,1:stp]
    numeric_columns <- grep("^\\d", names( HmdDets2) ) 
    hix = names( HmdDets2)[numeric_columns]
    
    # SET UP PARAMS ####
    RRPCAsumOUT = NULL # summary of percentiles for each site
    fqr  = "LF"  
    DC = Sys.Date()
    rrpca1 = -0.5 #default settings
    LB = "LF" #what label do you want to indicate on the ouutput file, LF = low frequency
    Nv   =  HmdDets2[, numeric_columns]  #dB values
    NvP  = 10^(Nv/20)     #pressure values
    nvDate = HmdDets2$dateTime
    sampleHours = nrow(NvP)
    
    # run model ####
    lamd = max(NvP)^(-rrpca1) 
    nvpcaTOL = rrpca(NvP)
    
    # save output ####
    save(nvpcaTOL, file = paste0(dirOut, "\\", site,  "_RrpcaResults_100-1000Hz", DC, ".Rda") )
    
    
  }
  
} else {
  
  dirOut = dirIn
  # colnames(HmdTrim)
  HmdTrim$mth <- month(HmdTrim$dateTime)
  HmdDets = HmdTrim
  idNA = ( which(is.na(HmdDets)))
  #truncate frequency to 100-1000 Hz
  stp = which( names( HmdDets) ==  "1001.2")
  HmdDets2 = HmdDets[,1:stp]
  numeric_columns <- grep("^\\d", names( HmdDets2) ) 
  hix = names( HmdDets2)[numeric_columns]
  
  # SET UP PARAMS ####
  RRPCAsumOUT = NULL # summary of percentiles for each site
  fqr  = "LF"  
  DC = Sys.Date()
  rrpca1 = -0.5 #default settings
  LB = "LF" #what label do you want to indicate on the ouutput file, LF = low frequency
  
  Nv   =  HmdDets2[, numeric_columns]  #dB values
  NvP  = 10^(Nv/20)     #pressure values
  nvDate = HmdDets2$dateTime
  sampleHours = nrow(NvP)
  
  ## RRPCA ####
  # Robust principal components analysis separates a matrix into a low-rank plus sparse component
  #a method for the robust separation of a rectangular (m, n) matrix A into a low-rank component L 
  # and a sparse component S
  lamd = max(NvP)^(-rrpca1) #default settings
  nvpcaTOL = rrpca(NvP)
  save(nvpcaTOL, file = paste0(dirOut, "\\", siteN,  "_RrpcaResults_100-1000Hz", DC, ".Rda") )

}


# Reload Results a & Plot ####
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

# sum of sparse across frequencies for each minute
SPsum = as.data.frame  ( rowSums( abs ( Sp ) ) )
colnames(SPsum) = 'SPsum'
min(SPsum$SPsum)

## LABEL HMD files ####
HmdDets$LowRanK = as.numeric( as.character(LRdiff$LRdiff ) )
HmdDets$Sparce  = as.numeric( as.character(SPsum$SPsum  ) )
HmdDets$LRfq = LRfq$LRfq

### scatter by season, category & RRPCA values ####
# ggplot(data = HmdDets, aes(x = LowRanK, Sparce, color = LRfq)) +
#   geom_point(alpha = .1) + 
#   scale_x_log10() +   scale_y_log10() +
#   facet_wrap(~mth) +
#   ggtitle("How soundscape conditions group by season")+
#   theme_minimal()+
#   theme(text = element_text(size = 16) )


### scatter by season, category & RRPCA values ####
HmdDets$Day = as.Date(HmdDets$dateTime)


# Custom function to calculate median and standard error
custom_fun <- function(x) {
  n <- length(x)
  c(median = median(x), se = sd(x) / sqrt(n))
}
quantile_25_75 <- function(x) {
  quantile(x, probs = c(0.25, 0.75))
}

result_dplyr <- HmdDets %>%
  group_by(Day, !!sym(voi)) %>%
  summarise(
    LowRankDiff = mean(LowRanK, na.rm = TRUE),
    se = sd(LowRanK, na.rm = TRUE) / sqrt(n())
  )
as.data.frame(result_dplyr)

ggplot(data = result_dplyr, aes(x = Day, y = LowRankDiff, color = !!sym(voi) )) +
  geom_point(size = 2) + 
  ggtitle("How residual soundscape conditions changes")+
  theme_minimal()+
  theme(text = element_text(size = 16) )

ggplot(data = result_dplyr, aes(x = Day, y = LowRankDiff, color = as.factor(!!sym(voi)))) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = LowRankDiff - se, ymax = LowRankDiff + se), width = 0.2) +
  ggtitle("How residual soundscape condition changes") +
  xlab("")+ ylab("Summed difference between low-rank and origional data") +
  labs(subtitle = "Higher values indicate more transient sounds present") +
  theme_minimal() +
  theme(text = element_text(size = 16))

# Calculate Residual sound levels for seasons, months, and daily across time
## ? ADD 25/75 ####
LrDB$Day = HmdDets$Day
LrDB$mth = HmdDets$mth  
LrDB$site = HmdDets$site  
RSoundscape = aggregate( LrDB[,hix], by = list(Season = get(voi, LrDB)), FUN = (median) )
RSoundscape = as.data.frame(RSoundscape)
melted_df50 <- reshape2::melt(RSoundscape, id.vars = c("Season"),  measure.vars = hix ) 
melted_df50$Fq = as.numeric(as.character(melted_df50$variable))

ggplot()+
  geom_line(data = melted_df50, aes(x = Fq, y = value, color = as.factor(Season), group = as.factor(Season)), linewidth = 2 ) +
  scale_x_log10() +
  theme_minimal() +
  theme(
    text = element_text(size = 12),  # Set all text to size 16
    axis.title = element_text(size = 12),  # Axis titles
    axis.text = element_text(size = 12),  # Axis labels
    legend.text = element_text(size = 12),  # Legend text
    strip.text = element_text(size = 12)  # Facet labels
  ) +
  labs(
    title = "",
    x = "Frequency (Hz)",
    y =expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)"))
  )

# START HERE ####
# What about different of original from the median low rank?
# subtract each row and frequency column by the residual matrix- 
# do I want the absolute value? maybe not because values 
# below the mean might be interesting too- an increase in less than might mean
# a decrease in some source... 
# ORG - LR (positive- means higher than LR, which should be the case??) 



LRdiff = as.data.frame ( rowSums( abs ( (LrDB - Nv) ) ) )


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
