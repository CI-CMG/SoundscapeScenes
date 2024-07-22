# PLots and summaries to compare results across acoustic scene analyses

# specific detections + wind, RRPCA, cluster analysis
# written for Ocean Sciences 2024 poster
# only SB03

# UP NEXT- extract PSD percentile values for each acoustic scene category

rm(list=ls()) 

#library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)
library(tidyverse)
library(ggmosaic)

# PARAMS ####
pltf = 0
fqr = "LF"  #append this to output names

# READ IN FILES ####
## HMD+ ####
inDir = (  "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\SoundCoop_AcousticScene\\combineFiles_AcousticScene" )
inFiles = list.files( inDir, pattern = "HMDdets_RpcaSite_SB03_1", full.names = T)
basename(inFiles)

inDirCC = paste0("G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\SoundCoop_AcousticScene\\ClusterAnalysis\\outputCF" )

talSSD = NULL
AS = NULL

for (f in 1: length(inFiles) ) {# f = 1 # one site at a time
  load( inFiles[f]) # head(HMDdet)
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 3) #site name
  dp =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 4) #site name
  # min(HMDdet$dateTime) max(HMDdet$dateTime)
  
  ##  WIND ####
  # only SB03
  inDirW = paste0(inDir,  "\\wind" )
  inFilesW = list.files( inDirW, pattern = "env", full.names = T)
  inFilesW = inFilesW[grepl(st, inFilesW)] #remove median spectra files
  if (length(inFilesW) >0 ) {
    WINDdata = read.csv(inFilesW)
    #head(WINDdata)
    WINDdata$time = gsub("[+]00:00", "", WINDdata$time )
    WINDdata$dateTime = as.POSIXct( WINDdata$time, format = "%Y-%m-%d %H:%M:%S" , tz = "GMT")
    WINDdata$yr = year(WINDdata$dateTime)
    min(WINDdata$dateTime)
    max(WINDdata$dateTime)
    # WINDdata = WINDdata[WINDdata$yr == yoi,]
    ixd = which(!is.na(WINDdata$wind_speed))
    WINDdata = WINDdata[ixd, ]
    # min(WINDdata$dateTime)
    # max(WINDdata$dateTime)
    
    HMDdet$wind_speed = NA
    #HMDdet$sea_water_temperature = NA
    #HMDdet$sea_surface_wave_significant_height = NA
    for (ww in 1:nrow(WINDdata) ){ # ww = 2164 
      idx = which (HMDdet$dateTime >= WINDdata$dateTime[ww] & HMDdet$dateTime < (WINDdata$dateTime[ww] + (60*60) ) )  
      # if(length(idx) > 0) { cat(ww, "\n")}
      # HMDdet$dateTime[idx]
      
      HMDdet$wind_speed[idx] =  WINDdata$wind_speed[ww] 
      #HMDdet$sea_water_temperature[idx] =  WINDdata$sea_water_temperature[ww] 
      #HMDdet$sea_surface_wave_significant_height[idx] =  WINDdata$sea_surface_wave_significant_height[ww] 
    }
  } # unique(HMDdet$wind_speed)
  idxW = which( HMDdet$wind_speed > 10)
  HMDdet$windCat = "low"
  HMDdet$windCat[idxW] = "high"
  
  HMDdet0 = HMDdet # for testing- HMDdet = HMDdet0
  
  ## CLUSTER RESULTS
  inFilesCC = list.files( inDirCC, pattern = "cluster", full.names = T)
  clust = read.csv(inFilesCC)
  colnames(clust) = c("year","month","day","hour","minute","second","CC")
  clust$dateTime <- as.POSIXct(   paste(clust$year, clust$month, clust$day,
                                        clust$hour, clust$minute, clust$second),   format = "%Y %m %d %H %M %S", tz = "GMT") 
  clust = dplyr::arrange(clust, dateTime)
  
  merged_df = merge(HMDdet, clust[, c("dateTime", "CC")], by = "dateTime", all.x = TRUE)
  unique(merged_df$CC)
  # some rows with NA when merged... why would we not have cluster results for some of the minutes???
  # merged_df$dateTime[222] ... it is missing
  #truncate cluster to only time in the current deployment
  #idx = which (clust$dateTime >= min(HMDdet$dateTime)  & clust$dateTime <= max(HMDdet$dateTime) )  
  #clustT = clust[idx,]
  
  ## ACOUSTIC SCENES ####
  ### SSD: Specific detectors + wind ####
  HMDdet = merged_df
  HMDdet$SSD = paste(HMDdet$windCat, HMDdet$Category, sep = "_" )
  idNa = which ( is.na(HMDdet$wind_speed) )
  HMDdet$SSD[idNa] = "Unk"
  HMDdet$mth = month(HMDdet$dateTime)
  
  tal = as.data.frame( HMDdet %>% group_by(SSD, mth) %>% tally() )
  tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
  tal$Site = paste(st,dp,sep = "_" )
  # tal # copied to google sheets to make a donut plot
  # https://docs.google.com/spreadsheets/d/11mHgePBe4sNZDqKprJI9KkcAXtcGO1Hxn77MnvbgHqg/edit#gid=1366513411
  talSSD = rbind(talSSD, tal)
  
  ### RRPCA ####
  # + lowrank sum above 25th percentile of all values
  thrSP = round( quantile (HMDdet$Sparce, .75, na.rm = T )) # expect sparce to be higher
  thrLR = round( quantile (HMDdet$LowRanK, .25,na.rm = T )) # but low rank not as much because wind speed might be driving the super high values
  
  # without considering conditions
  indx = ( which( HMDdet$LowRanK > thrLR & HMDdet$Sparce > thrSP  ) )
  HMDdet$RRPCA1 = "Ambient"
  HMDdet$RRPCA1[indx] = "Transient"
  
  # considering SSD conditions
  HMDdet$RRPCA = HMDdet$Category 
  idxAnth = ( which( HMDdet$LowRanK > thrLR & HMDdet$Sparce > thrSP & HMDdet$Category == "Anthro") )
  HMDdet$RRPCA[idxAnth] = "Anthro+"
  idxAmb = ( which( HMDdet$LowRanK > thrLR & HMDdet$Sparce > thrSP & HMDdet$Category == "Ambient") )
  HMDdet$RRPCA[idxAmb] = "Ambient+"
  idxBio = ( which( HMDdet$LowRanK > thrLR & HMDdet$Sparce > thrSP & HMDdet$Category == "Bio") )
  HMDdet$RRPCA[idxBio] = "Bio+"
  unique( HMDdet$RRPCA )
  HMDdet$RRPCAw = paste(HMDdet$windCat, HMDdet$RRPCA, sep = "_" )
  
  save(HMDdet, file = paste0(inDir, "\\HMD_DetsWindRpcaCluster_", st, "_", dp, ".Rda") )
  
  ## PLOT: mosaic plot ####
  ggplot(data = HMDdet) +
    geom_mosaic(aes(x=product(SSD), fill = RRPCAw)) + 
    theme_minimal()
  
  HMDdet$clust = as.character( HMDdet$CC )
  idNa = which ( is.na(HMDdet$clust) )
  HMDdet$clust[idNa] = "Unk"
  
  ggplot(HMDdet, aes(fill = RRPCAw, x = clust)) +
    geom_bar(position="fill") +
    theme_minimal()
  
  ggplot(data = HMDdet) +
    geom_mosaic(aes(x=product(clust), fill = RRPCAw)) + 
    theme_minimal()
  
  ## COMBINE DATA- just scenes
  AS = rbind ( AS, select(HMDdet, c('dateTime','SSD','windCat', 'RRPCA', 'RRPCAw','clust','wind_speed') ) )
  
}

write.csv(talSSD, file = paste0(inDir, "\\SSDsummary_", st,".csv") )
write.csv(AS, file = paste0(inDir, "\\HMD_DetsWindRpcaCluster_", st,".csv") )
save(AS, file = paste0(inDir, "\\HMD_DetsWindRpcaCluster_", st,".Rda") )

# FULL YEAR PLOTS ####
##SSD for pie chart--- samples over multiple months
month_talSSD = talSSD %>%
  group_by(mth, SSD) %>%
  summarize(Sum_Value = sum(n))

total_talSSD = talSSD %>%
  group_by(SSD) %>%
  summarize(Sum_Value = sum(n))
write.csv(month_talSSD, file = paste0(inDir, "\\SSDsummary_", st,".csv") )

sum(total_talSSD$Sum_Value)/(60*24)
# plot in google sheets

##SSD vs RRPCA
ggplot(data = AS) +
  geom_mosaic(aes(x=product(SSD), fill = RRPCAw)) + 
  theme_minimal()

AS$mth = month(AS$dateTime)
ggplot(data = AS[AS$mth==7,]) +
  geom_mosaic(aes(x=product(SSD), fill = RRPCAw)) + 
  theme_minimal()

##RRPCAw vs cluster
unique( AS$clust )
#remove NAs-- too big
length( which(AS$clust=="Unk" ) )/nrow(AS)
idx = ( which(AS$clust!="Unk" ) )
ASt = AS[idx,]
ggplot(data = ASt) +
  geom_mosaic(aes(x=product(clust), fill = RRPCAw)) + 
  theme_minimal()

ggplot(ASt, aes(fill = RRPCAw, x = clust)) +
  geom_bar(position="fill") +
  theme_minimal()

ggplot(ASt[ASt$mth==7,], aes(fill = RRPCAw, x = clust)) +
  geom_bar(position="fill") +
  theme_minimal()



# READ IN outputs
inDir = (  "F:\\SanctSound\\analysis\\combineFiles_AcousticScene" )
total_talSSD = read.csv(paste0(inDir,"\\SSDsummary_SB03.csv"))
#summarize within category
total_talSSD %>%
  group_by(SSD) %>%
  summarize(Sum_Value = sum(Sum_Value))


# REMOVED FROM THIS ANALYSIS
## PLOT: acoustic Scene over time ####
HMDdet = HMDdet %>%  mutate(test = case_when(SSD == lag(SSD) ~ "No", TRUE ~ "Yes"))
idx = which(HMDdet$test == "Yes") #start of new scene
AS = NULL
AS$Start    = HMDdet$dateTime[idx]
AS$Category = HMDdet$SSD[idx]
AS$End      = c(  HMDdet$dateTime[ idx[2:length(idx) ] - 1 ] , HMDdet$dateTime[nrow(HMDdet)]  )
AS$Hours    = as.numeric( as.character( difftime(AS$End, AS$Start, units = "hours") ))
AS$mth = month(AS$Start)
AS = as.data.frame (AS)

p1 = ggplot(AS, aes(x=Start, xend=End, y=Category, yend=Category, color = Hours)) +
  geom_segment() +
  theme_bw()+ 
  scale_y_discrete(limits=rev)+ 
  geom_segment(size=10) +
  xlab("")+  ylab("")+ 
  scale_color_gradientn(colours = viridis(10))+
  theme(  axis.text.y = element_text(size = 10, colour="black"),
          axis.text.x = element_text(size = 10, colour="black"),
          plot.caption = element_text(size = 8) )
p1

#ggsave(p1, file = paste0(inDir, "\\AcousticScene_",fqr, "_" , st,".png"), width = 1500, height = 700, units = "px")

# HMD values for each scene ####
## FOR no wind sites ####
endCol = ncol(HMDdet) - 15 # 
colnames(HMDdet)[endCol]
colnames(HMDdet)[2]
FQlow = as.numeric( as.character( gsub("X","", colnames(HMDdet)[2:endCol]) ) )

unique(HMDdet$Category3)
HMDdet$Category4[HMDdet$Category3 == "low_Ambient_Ambient"] = "Low wind"
HMDdet$Category4[HMDdet$Category3 == "low_Ambient_Ambient+"] = "Low transient"
HMDdet$Category4[HMDdet$Category3 == "Unk"] = "Unknown"
HMDdet$Category4[HMDdet$Category3 == "high_Ambient_Ambient+"] = "High transient"
HMDdet$Category4[HMDdet$Category3 == "high_Ambient_Ambient"] = "High wind"
HMDdet$Category4[HMDdet$Category3 == "low_Anthro_Anthro"] = "Low vessels"
HMDdet$Category4[HMDdet$Category3 == "low_Bio+Anthro_Bio+Anthro"] = "Low vessesl + transient"
HMDdet$Category4[HMDdet$Category3 == "high_Bio+Anthro_Bio+Anthro"] = "High vessesl + transient"
HMDdet$Category4[HMDdet$Category3 == "high_Anthro_Anthro"] = "High vessels"

unique(HMDdet$Category4)
# wind conditions
LWi <- HMDdet %>% filter(Category4 =="Low wind")
LWiM = LWi %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
LWiM$LRfq = as.numeric( as.character( gsub("X","", LWiM$key ) ) ) 

HWi <- HMDdet %>% filter(Category4 =="High wind")
HWiM = HWi %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
HWiM$LRfq = as.numeric( as.character( gsub("X","", HWiM$key ) ) ) 
#transient conditions
LTr <- HMDdet %>% filter(Category4 =="Low transient")
LTrM = LTr %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
LTrM$LRfq = as.numeric( as.character( gsub("X","", LTrM$key ) ) ) 

HTr <- HMDdet %>% filter(Category4 =="High transient")
HTrM = HTr %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
HTrM$LRfq = as.numeric( as.character( gsub("X","", HTrM$key ) ) ) 

LTV <- HMDdet %>% filter(Category4 =="Low vessesl + transient")
LTVM = LTV %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
LTVM$LRfq = as.numeric( as.character( gsub("X","", LTVM$key ) ) ) 

LVe <- HMDdet %>% filter(Category4 =="Low vessels")
LVeM = LVe %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
LVeM$LRfq = as.numeric( as.character( gsub("X","", LVeM$key ) ) ) 


# START HERE ####
ggplot(LWiM, aes(x=LRfq , y=med.x) )  +
  #wind
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  geom_line(data = HWiM, aes(x=LRfq , y=med.x), linewidth = 2, color = "black") +
  #transient
  geom_line(data = LTrM, aes(x=LRfq , y=med.x), linewidth = 2, color = "lightblue") +
  geom_line(data = HTrM, aes(x=LRfq , y=med.x), linewidth = 2, color = "royalblue") +
  #vessels
  geom_line(data = LVeM, aes(x=LRfq , y=med.x), linewidth = 2, color = "indianred1") +
  #vessel + transients
  geom_line(data = LTVM, aes(x=LRfq , y=med.x), linewidth = 2, color = "tan1") +
  
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(50,95)) +
  theme_bw()+ 
  ggtitle(paste0( "Acoustic Scene | ", st) ) +
  theme(legend.position="top")+
  theme(text = element_text(size =16) )

#ggplotly(p1)

## FOR no wind sites ####
endCol = ncol(HMDdet) - 12 # 
colnames(HMDdet)[endCol]
colnames(HMDdet)[2]
FQlow = as.numeric( as.character( gsub("X","", colnames(HMDdet)[2:endCol]) ) )
HMDdet$Category4[HMDdet$Category3 == "Bio+Anthro_Bio+Anthro"] = "Vessels + Transient"
HMDdet$Category4[HMDdet$Category3 == "Anthro_Anthro"] = "Vessels"
HMDdet$Category4[HMDdet$Category3 == "Ambient_Ambient+"] = "Transient"
HMDdet$Category4[HMDdet$Category3 == "Ambient_Ambient"] = "Wind"
ucats = unique(HMDdet$Category4)

VT <- HMDdet %>% filter(Category4 =="Vessels + Transient")
VTM = VT %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
VTM$LRfq = as.numeric( as.character( gsub("X","", VTM$key ) ) ) 

Ve <- HMDdet %>% filter(Category4 =="Vessels")
VeM = Ve %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
VeM$LRfq = as.numeric( as.character( gsub("X","", VeM$key ) ) ) 

Tr <- HMDdet %>% filter(Category4 =="Transient")
TrM = Tr %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
TrM$LRfq = as.numeric( as.character( gsub("X","", TrM$key ) ) ) 

Wi <- HMDdet %>% filter(Category4 =="Wind")
WiM = Wi %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
WiM$LRfq = as.numeric( as.character( gsub("X","", WiM$key ) ) ) 

ggplot(WiM, aes(x=LRfq , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  
  geom_line(data = TrM, aes(x=LRfq , y=med.x), linewidth = 2, color = "royalblue") +
  #geom_line(data = TrM, aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "blue" , linetype="dotted") + 
  #geom_line(data = TrM, aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "blue", linetype="dotted") +
  
  geom_line(data = VeM, aes(x=LRfq , y=med.x), linewidth = 2, color = "red3") +
  #geom_line(data = VeM, aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "red" , linetype="dotted") + 
  #geom_line(data = VeM, aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "red", linetype="dotted") +
  
  geom_line(data = VTM, aes(x=LRfq , y=med.x), linewidth = 2, color = "darkorange") +
  #geom_line(data = VTM, aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "orange" , linetype="dotted") + 
  #geom_line(data = VTM, aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "orange", linetype="dotted")+

  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(60,90)) +
  theme_bw()+ 
  ggtitle(paste0( "Acoustic Scene | ", st) ) +
  theme(legend.position="top")+
  theme(text = element_text(size =16) )

#ggplotly(p1)


# not used ####
Ambient = rbind (Ambient, tst[tst$Category == "Ambient",] )
Anthro = rbind (Anthro, tst[tst$Category == "Anthro",] )
Bio = rbind (Bio, tst[tst$Category == "Bio",] )
BioAnthro = rbind (BioAnthro, tst[tst$Category == "Bio+Anthro",] )

totalMinsAmb = totalMinsAmb + nrow(tst[tst$Category == "Ambient",])
totalMinsBio = totalMinsBio + nrow(tst[tst$Category == "Bio",])
totalMinsBoth = totalMinsBoth + nrow(tst[tst$Category == "Bio+Anthro",])
totalMinsAnthro= totalMinsAnthro + nrow(tst[tst$Category == "Anthro",])

# START HERE ####

p2 = ggplot(Anthro, aes(x=Fq , y=mean.x, color = Site) )  +
  geom_line( size = 1) +
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(50,100)) +
  theme_bw()+ 
  ggtitle(paste0( "Anthropogenic sources (only low-frequency)")) +
  theme(legend.position="top")+
  theme(text = element_text(size =10) )
p2
ggsave(p2, file = paste0(inDir, "\\ASspectra_Anthro_",fqr, "_", st,".png"), width = 1500, height = 1000, units = "px")


p2 = ggplot(Bio, aes(x=Fq , y=mean.x, color = Site) )  +
  geom_line( size = 1) +
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(50,100)) +
  theme_bw()+ 
  ggtitle(paste0( "Biological sources (only low-frequency)")) +
  theme(legend.position="top")+
  theme(text = element_text(size =10) )
p2
ggsave(p2, file = paste0(inDir, "\\ASspectra_Bio_",fqr, "_", st,".png"), width = 1500, height = 1000, units = "px")


p2 = ggplot(BioAnthro, aes(x=Fq , y=mean.x, color = Site) )  +
  geom_line( size = 1) +
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(50,100)) +
  theme_bw()+ 
  ggtitle(paste0( "Biological+Anthropogenic sources (only low-frequency)")) +
  theme(legend.position="top")+
  theme(text = element_text(size =10) )
p2
ggsave(p2, file = paste0(inDir, "\\ASspectra_BioAnthro_",fqr, "_", st,".png"), width = 1500, height = 1000, units = "px")

