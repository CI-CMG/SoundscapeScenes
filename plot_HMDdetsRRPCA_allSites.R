# PLot spectra across sites for acoustic scene categories

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)
library(tidyverse)


inDir = (  "F:\\SanctSound\\analysis\\combineFiles_AcousticScene" )
inFiles = list.files( inDir, pattern = "HMDdetLF_Spectra", full.names = T)
pltf = 0
fqr = "LF"  #append this to output names
Ambient = NULL
Anthro = NULL
Bio = NULL
BioAnthro = NULL

# uAS = unique(tst$Category)
uAS = c( "Ambient"   , "Anthro" ,    "Bio" ,       "Bio+Anthro" )

for (f in 1: length(inFiles)) { # f = 6 for testing
  
  load( inFiles[f])
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 3) #site name
  tst$Site = st
  
  Ambient = rbind (Ambient, tst[tst$Category == "Ambient",] )
  Anthro = rbind (Anthro, tst[tst$Category == "Anthro",] )
  Bio = rbind (Bio, tst[tst$Category == "Bio",] )
  BioAnthro = rbind (BioAnthro, tst[tst$Category == "Bio+Anthro",] )
  
  
}

## PLOT 2: median spectra ####
p2 = ggplot(Ambient, aes(x=Fq , y=mean.x, color = Site) )  +
  geom_line( size = 1) +
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(50,100)) +
  theme_bw()+ 
  ggtitle(paste0( "Ambient")) +
  theme(legend.position="top")+
  theme(text = element_text(size =10) )
p2
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

