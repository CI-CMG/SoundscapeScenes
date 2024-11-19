# NEXT

rm(list=ls()) 
# LIBRARIES ####
library(shiny)
library(tidyverse)
library(dplyr)
library(ncdf4)
library (ggplot2)

# PARAMS ####
LB = "LF" #what label do you want to indicate on the ouutput file, LF = low frequency
DC = Sys.Date()

# DIRS ####
gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
dirOut =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS" )

# HMD+ FILES ####
siteN = "AU_CH01"
HMDversion = paste0("HmdLabels_", LB, "_2024-06-27")
inDir = paste0( dirOut, "\\",siteN )
files = list.files(inDir, pattern = HMDversion, full.names = T, recursive = T)
file_names = basename(files)
load(files)

## SUMMARY PLOT ####
# labels overtime
detAllt <- detAll[!detAll$sourceType %in% c("NothingIceOK", "AnyOdonto", "AnyDblKnk", "AnyCet", "AnyBel", "AnyAnthro","AnyPinn", "AnyBaleen","AnyBio"),]

ggplot(detAllt, aes(x=Start, xend=End, y=sourceType, yend=sourceType, color = season)) +
  geom_segment() +  theme_bw() + 
  geom_segment(size=8) +
  xlab("")+  ylab("") + 
  ggtitle (paste("Soundscape Labels")) +
  theme(  axis.text.y = element_text(size = 12, colour="black"),
          axis.text.x = element_text(size = 12, colour="black"),
          plot.caption = element_text(size = 8) ,
          plot.title = element_text(size = 16))


# spectra by unique labels
numeric_columns = grep("^\\d", names(HmdDets) )  
hix = names(HmdDets)[numeric_columns]
useason = unique(HmdDets$season)
dfT = NULL
for (s in 1:length(useason)) {
  tmpD  = HmdDets %>% filter(season == useason[s])
  tmpP = tmpD %>% gather(key, value, numeric_columns) %>% group_by(key) %>% 
    dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                     med.x  = quantile(value, probs = 0.5),
                     upper.x = quantile(value, probs = 0.75))
  tmpP$Category = useason[s]
  dfT = rbind(dfT, tmpP) 
  rm(tmpD, tmpP)
}

ggplot(dfT, aes(x=as.numeric( as.character(key) ) , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  facet_wrap(~Category) +
  scale_x_log10() +  ylab("1-min PSD median") + xlab("HMD Frequency (Hz)")+
  ylim(c(50,95))  +   theme_bw()+ 
  ggtitle (paste("Soundscape Metrics by Seasons")) +
  theme(text = element_text(size = 16) )
