# PLot spectra across sites for acoustic scene categories, including rrpca for ambient

#plot and export for NNET

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)
library(tidyverse)

# READ IN COMPILED FILES ####
inDir = (  "F:\\SanctSound\\analysis\\combineFiles_AcousticScene" )
inFiles = list.files( inDir, pattern = "HMDdetsRpca_", full.names = T)
inFiles
pltf = 0
fqr = "LF"  #append this to output names

# PLOT BY SITE ###
# see plot_HMDdets.R

# PLOT BY ACOUSTIC SCENE ####
Ambient = NULL
Anthro = NULL
Bio = NULL
BioAnthro = NULL
uAS = c( "Ambient"   , "Anthro" ,    "Bio" ,       "Bio+Anthro" )

#1. combine all site data together for each scene
totalMins = 0
totalMinsAmb = 0
totalMinsBio = 0
totalMinsBoth = 0
totalMinsAnthro= 0

for (f in 1: length(inFiles)) { # f = 8 for testing
  
  load( inFiles[f])
  head(tst)
  
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 2) #site name
  tst$Site = st
  totalMins = totalMins + nrow(tst)
  
  #WIND-- ONLY SB03
  inDirW = (  "F:\\SanctSound\\analysis\\ERDAP_wind" )
  inFilesW = list.files( inDirW, pattern = "env", full.names = T)
  WINDdata = read.csv(inFilesW)
  
  #as.data.frame(colnames( WINDdata) )
  WINDdata$time = gsub("[+]00:00", "", WINDdata$time )
  WINDdata$dateTime = as.POSIXct( WINDdata$time, format = "%Y-%m-%d %H:%M:%S" , tz = "GMT")
  WINDdata$yr = year(WINDdata$dateTime)
  WINDdata = WINDdata[WINDdata$yr == "2020",]
  ixd = which(!is.na(WINDdata$wind_speed))
  WINDdata = WINDdata[ixd, ]
  
  tst$wind_speed = NA
  tst$sea_water_temperature = NA
  tst$sea_surface_wave_significant_height = NA
  
  for (ww in 1:nrow(WINDdata) ){ # ww = 3000
    idx = which (tst$dateTime >= WINDdata$dateTime[ww] & tst$dateTime < (WINDdata$dateTime[ww] + (60*60) ) )  
    tst$wind_speed[idx] =  WINDdata$wind_speed[ww] 
    tst$sea_water_temperature[idx] =  WINDdata$sea_water_temperature[ww] 
    tst$sea_surface_wave_significant_height[idx] =  WINDdata$sea_surface_wave_significant_height[ww] 
  }
  #unique(tst$wind_speed)
  
  #ADD Ambient+ category
  #quantile (tst$Sparce, c(0.25,0.50,0.75),na.rm = T )  # sum over all frequencies
  #quantile (tst$LowRanK, c(0.25,0.50,0.75),na.rm = T ) # sum difference over all frequencies
  #quantile (tst$LRfq, c(0.25,0.50,0.75),na.rm = T )    # which frequency was the most differen-- intersting always below 200 HZ- whales?
  
  # WHAT IS THE THRESHOLD? 
  # ambient+ lowrank sum above 25th percentile of all values...
  thrSP = round( quantile (tst$Sparce, .75, na.rm = T )) # expect sparce to be higher
  thrLR = round( quantile (tst$LowRanK, .25,na.rm = T )) # but low rank not as much because wind speed might be driving the super high values
  
  unique( tst$Category)
  tst$Category2 = tst$Category
  idx =  ( which( tst$Sparce > thrSP) ) # length(idx)
  idx =  ( which( tst$LowRanK > thrLR & tst$Sparce > thrSP) ) # length(idx)
  #unique( tst$Category[idx] )
  tst$Category2[idx] = "Ambient+"
  (unique( tst$Category2))
  
  #add wind category
  idxW = which( tst$wind_speed > 10)
  tst$windCat = "low"
  tst$windCat[idxW] = "high"
  unique( tst$windCat)
  tst$Category3 = paste(tst$windCat, tst$Category2, sep = "_" )
  
  #PLOT temporal patterns with Ambient+ category
  # get start and end of different AS, so segment works for plotting
  tst = tst %>%  mutate(test = case_when(Category3 == lag(Category3) ~ "No", TRUE ~ "Yes"))
  idx = which(tst$test == "Yes") #start of new scene
 
  AS = NULL
  AS$Start    = tst$dateTime[idx]
  AS$Category = tst$Category3[idx]
  AS$End      = c(  tst$dateTime[ idx[2:length(idx) ] - 1 ] , tst$dateTime[nrow(tst)]  )
  AS$Hours    = as.numeric( as.character( difftime(AS$End, AS$Start, units = "hours") ))
  
  tal = as.data.frame( tst %>% group_by(Category3) %>% tally() )
  tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
  tal
  AS$mth = month(AS$Start)
  AS = as.data.frame (AS)
  
  ## PLOT 1: acoustic Scene over time ####
  p1 = ggplot(AS, aes(x=Start, xend=End, y=Category, yend=Category, color = Hours)) +
    geom_segment() +
    theme_bw()+ 
    scale_y_discrete(limits=rev)+ 
    geom_segment(size=10) +
    xlab("")+  ylab("")+ 
    ggtitle(paste("Acoustic Scenes at ", st, " (1-min, RRPCA)", sep = "") )  +
    labs(caption = (paste0("% time in each category: ", 
                           tal$Category[1], "=", round(tal$PerTime[1]), "% | ", 
                           tal$Category[2], "=", round(tal$PerTime[2]), "% | ",
                           tal$Category[3], "=", round(tal$PerTime[3]), "% | ",
                           tal$Category[4], "=", round(tal$PerTime[4]), "% | ",
                           tal$Category[5], "=", round(tal$PerTime[5]), "% | ",
                           tal$Category[6], "=", round(tal$PerTime[6]), "%"))) +
    scale_color_gradientn(colours = viridis(10))+
    #facet_wrap(~mth) +
    theme(  axis.text.y = element_text(size = 10, colour="black"),
            axis.text.x = element_text(size = 10, colour="black"),
            plot.caption = element_text(size = 8) )
  
  p1
  
  #ggsave(p1, file = paste0(inDir, "\\AcousticScene_",fqr, "_" , st,".png"), width = 1500, height = 700, units = "px")
  
  Ambient = rbind (Ambient, tst[tst$Category == "Ambient",] )
  Anthro = rbind (Anthro, tst[tst$Category == "Anthro",] )
  Bio = rbind (Bio, tst[tst$Category == "Bio",] )
  BioAnthro = rbind (BioAnthro, tst[tst$Category == "Bio+Anthro",] )
  
  totalMinsAmb = totalMinsAmb + nrow(tst[tst$Category == "Ambient",])
  totalMinsBio = totalMinsBio + nrow(tst[tst$Category == "Bio",])
  totalMinsBoth = totalMinsBoth + nrow(tst[tst$Category == "Bio+Anthro",])
  totalMinsAnthro= totalMinsAnthro + nrow(tst[tst$Category == "Anthro",])
}
endCol = ncol(Ambient) - 11 # 
colnames(Ambient)[endCol]
colnames(Ambient)[1]
FQlow = as.numeric( as.character( gsub("X","", colnames(Ambient)[2:endCol]) ) )

# #2.1 AMBIENT- split by RRPCA results-- 
quantile (Ambient$Sparce, c(0.25,0.50,0.75) )  # sum over all frequencies
quantile (Ambient$LowRanK, c(0.25,0.50,0.75) ) # sum difference over all frequencies
quantile (Ambient$LRfq, c(0.25,0.50,0.75) )    # which frequency was the most differen-- intersting always below 200 HZ- whales?

# WHAT IS THE THRESHOLD? 
thrLR = round( quantile (Ambient$LowRanK, .50 ) )
# ambient+ = lowrank sum above 50th percentile of all values- 
thrSP = round( quantile (Ambient$Sparce, .25 ) )

Ambient$Category2 = Ambient$Category
idx =  ( which( Ambient$LowRanK > thrLR & Ambient$Sparce > thrSP) ) # length(idx)
Ambient$Category2[idx] = "Ambient+"

#3. get percentile values for each acoustic scene
AmbientPlus <- Ambient %>% filter(Category2 == "Ambient+")
Ambient0    <- Ambient %>% filter(Category2 != "Ambient+")
# check: nrow(AmbientPlus)/(60*24) + nrow( Ambient0 )/(60*24)

#percentile spectra for each category
AmbientPlusm = AmbientPlus %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   mean.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
AmbientPlusm$LRfq = as.numeric( as.character( gsub("X","", AmbientPlusm$key ) ) ) # ambient+

Ambient0m = Ambient0 %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   mean.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
Ambient0m$LRfq = as.numeric( as.character( gsub("X","", Ambient0m$key ) ) ) # ambient

# 4. plot of specta ####
p1 = ggplot(AmbientPlusm, aes(x=LRfq , y=mean.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  geom_line(data = Ambient0m, aes(x=LRfq , y=mean.x), linewidth = 2, color = "black") +
  geom_line(data = Ambient0m, aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "black" , linetype="dotted") + 
  geom_line(data = Ambient0m, aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "black", linetype="dotted") +
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(50,80)) +
  theme_bw()+ 
  ggtitle(paste0( "Ambient Types | gray = RRPCA, black = ambient")) +
  theme(legend.position="top")+
  theme(text = element_text(size =16) )
ggplotly(p1)

# summary of data-- made pie chart in google sheets

totalMins/ (60*24)
cat("Ambient: ", (totalMinsAmb/ totalMins) *100)
totalMinsAmb/ (60*24)
cat("Bio: ", (totalMinsBio/ totalMins) *100)
totalMinsBio/ (60*24)
cat("Anthro: ", (totalMinsAnthro/ totalMins) *100)
totalMinsAnthro/ (60*24)
cat("Both: ", (totalMinsBoth/ totalMins) *100 )
totalMinsBoth/ (60*24)
cat("Ambient+: ", (nrow(AmbientPlus)/totalMins) *100)
cat("Ambient: ", (nrow(Ambient0)/totalMins) *100)

# START HERE ####
ggsave(p2, file = paste0(inDir, "\\ASspectra_Ambient_",fqr, "_", st,".png"), width = 1500, height = 1000, units = "px")

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

