# combine HMD+ with clusters to compare results across acoustic scene analyses
# purpose is to review performance of clustering with scene categories
# calculates a concentration score to see how clustering worked

# WORKS one site at a time
#AU_CH01

# INPUT
# output of B_HmdDets.R
# triton cluster files in specifric verison (.csv)

# OUTPUT
#cluster # with date column (Rdat)

# NEXT

rm(list=ls()) 

# LIBRARIES ####
library(tidyr)
library(dplyr)

# PARAMS ####
siteN = "AU_CH01"
ver = "v5"
filepat = "_HmdLabels_LF_"
LB = "LF" #what label do you want to indicate on the ouutput file, LF = low frequency
DC = Sys.Date()

# GET DATA ####
gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"

## HMD+ ####
dirHmd =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS\\", siteN )
inFiles = list.files( dirHmd, pattern = filepat, recursive = F, full.names = T )
load( inFiles )
cat("HMD data: ", as.character(min(HmdDets$dateTime)),  as.character(max(HmdDets$dateTime)) )

## CLUSTERS ####
cldrive = "\\SoundCoop_AcousticScene\\ClusterAnalysis\\C_outputCC\\"
dirClus = paste0(gdrive, cldrive, siteN,"\\", ver)
inFilesCC = list.files( dirClus, pattern = paste0("^", siteN, "_.*_Bouts.csv$"), full.names = T)
clustAll = NULL
for (f in 1:length(inFilesCC )) {
  clust = read.csv(inFilesCC[f])
  clust$dateTime <- as.POSIXct(  clust$StartTime,   format = "%d-%b-%Y %H:%M:%S", tz = "GMT") 
  clust$season = sapply(strsplit(basename(inFilesCC[f]), "_"), "[[", 3)
  
  cat("Cluster data: ",  unique(clust$season), as.character(min(clust$dateTime)),  as.character(max(clust$dateTime)),"\n" )
  
  clusttmp = select(clust, c(season, dateTime, ClusterIDNumber) )
  clustAll = rbind(clustAll, clusttmp)
}
clustAll$ucluster = paste( clustAll$ClusterIDNumber, clustAll$season, sep = "_")
tal = as.data.frame( clustAll %>% group_by(season) %>% tally() )
tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )

# MERGE DATA ####
merged_df = merge(HmdDets, clustAll[, c("dateTime", "ucluster","ClusterIDNumber")], by = "dateTime", all.x = TRUE)
unique(merged_df$ClusterIDNumber)
idNA = which ( is.na(merged_df$ClusterIDNumber) ) # unique( df$ClusterIDNumber[idNA])
merged_df$ClusterIDNumber[idNA] = 0

clusts = as.data.frame( unique(merged_df$ucluster) )
tal = as.data.frame( merged_df %>% group_by(season, ClusterIDNumber) %>% tally() )
tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
tal #NAs are becausee data were isolated

# OUTPUT DATA ####
category_columns = which(colnames(merged_df) == "ucluster" ) 
category_columns1 = which(colnames(merged_df) == "ClusterIDNumber" ) 
season_columns = which(colnames(merged_df) == "season" ) 


outClust = as.data.frame( merged_df[, c(1, season_columns,category_columns, category_columns1) ] )
save(outClust, file = paste0(dirHmd, "\\", siteN,  "_HmdFeature-", ver, "_", LB ,"_",DC) ) 
