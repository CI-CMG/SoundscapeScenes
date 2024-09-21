# Define acoustic scenes from HMD+ data (Rdat)
#user decides on what detection types and categories

# WORKS one site at a time
#AU_CH01

# INPUT
#output of B_HmdLabels.R (Rdat)
#user defined acoustic categories

# OUTPUT
#Acoustic Scenes with date column and acoustic categories (Rdat)

# NEXT

rm(list=ls()) 
# LIBRARIES ####
library(tidyverse)
library(shiny)

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

# DEFINE ACOUSTIC CATEGORIES ####
## by site ####
if (siteN == "AU_CH01"){
  HmdDets$Category  = "unk"
  ## LOW FREQUENCY CATEGORIES ####
  idx = which(HmdDets$AnyBaleen >= 1 | HmdDets$AnyPinn >= 1)
  HmdDets$BioLF = 0
  HmdDets$BioLF[idx] = 1
  
  idx = which(HmdDets$AnyVess >= 1 | HmdDets$AnyAir >= 1)
  HmdDets$AntLF = 0
  HmdDets$AntLF[idx] = 1
  #HmdDets[idx[1],999:1020]
  
  # BIO
  idx = which( HmdDets$BioLF > 0 & HmdDets$AntLF == 0 ) 
  HmdDets$Category[idx] =  "2_BIO"
  # ANTHRO
  idx = which( HmdDets$BioLF == 0 & HmdDets$AntLF >= 1 ) 
  HmdDets$Category[idx] =  "3_ANTHRO"
  # BIO+ANTHRO
  idx = which( HmdDets$BioLF > 0 &  HmdDets$AntLF > 0) #bio + vess
  HmdDets$Category[idx] =  "4_BIO-ANTHRO"
  # AMBIENT
  idx = which(HmdDets$BioLF == 0 & HmdDets$AntLF == 0 ) 
  HmdDets$Category[idx] = "1_AMBIENT"
  cat("Acoustic Scenes in ",unlist(unique (HmdDets$season)), ":",
      unlist(unique (HmdDets$Category)) , "\n")
  talSeason   = as.data.frame( HmdDets %>% group_by(season,Category) %>% tally() )
  talSeason
  
  ## MORE SPECIFIC CATEGORIES ####
  HmdDets$Category2 = "unk" #more specific if needed
  # BIO-PINN
  idx = which( HmdDets$AntLF == 0  & HmdDets$AnyPinn > 0 & HmdDets$AnyBaleen == 0)
  HmdDets$Category2[idx ] =  "BIO-PINN"
  #unique(HmdDets$NothingIceOK[idx]) # THIS ONE HAS MORE ICE CATEGORIES
  
  # BIO-BAL
  idx = which( HmdDets$AntLF == 0  & HmdDets$AnyPinn == 0 & HmdDets$AnyBaleen > 0)
  HmdDets$Category2[idx ] =  "BIO-BAL"
  #unique(HmdDets$NothingIceOK[idx])
  
  # BIO-PINN-BAL
  idx = which( HmdDets$AntLF == 0  & HmdDets$AnyPinn > 0 & HmdDets$AnyBaleen > 0)
  HmdDets$Category2[idx] =  "BIO-PINN-BAL"
  #unique(HmdDets$NothingIceOK[idx])
  
  idx = which( HmdDets$AnyPinn > 0 & HmdDets$AnyBaleen > 0 & HmdDets$AntLF > 0) 
  HmdDets$Category2[idx ] =  "BIO-ANTHRO"
  #unique(HmdDets$NothingIceOK[idx])
  
  idx = which( HmdDets$AnyPinn == 0 & HmdDets$AnyBaleen == 0 & HmdDets$AntLF > 0) 
  HmdDets$Category2[idx ] =  "ANTHRO"
  #unique(HmdDets$NothingIceOK[idx])
  
  idx = which(HmdDets$NothingIceOK > 0, 
              HmdDets$AntLF == 0  & HmdDets$AnyPinn == 0 & HmdDets$AnyBaleen == 0) 
  HmdDets$Category2[idx] =  "AMBIENT-ICE"
  #unique(HmdDets$NothingIceOK[idx])
  
  idx = which(HmdDets$Category2 == "unk") 
  HmdDets$Category2[idx] =  "AMBIENT"
  #unique(HmdDets$NothingIceOK[idx])
  
  cat("Acoustic Scenes in ",unlist(unique (HmdDets$season)), ":",
      unlist(unique (HmdDets$Category2)) , "\n")
  talSeason   = as.data.frame( HmdDets %>% group_by(season,Category2) %>% tally() )
  talSeason
  
  ## OUTPUTS ####
  category_columns = which(colnames(HmdDets) == "Category" ) 
  category_columns1 = which(colnames(HmdDets) == "Category2" ) 
  season_columns = which(colnames(HmdDets) == "season" ) 
  
  HmdAS = as.data.frame( HmdDets[, c(1, season_columns,category_columns, category_columns1) ] )
  save(HmdAS, file = paste0(inDir, "\\", siteN, "_HmdScene_", LB, "_", DC) )
  
}

## SUMMARY PLOT ####
numeric_columns = grep("^\\d", names(HmdDets) )  
hix = names(HmdDets)[numeric_columns]
useason = unique(HmdDets$season)
uscene = unique(HmdDets$Category)
dfT = NULL
for (s in 1:length(uscene)) {
  tmpD  = HmdDets %>% filter(Category == uscene[s])
  tmpP = tmpD %>% gather(key, value, numeric_columns) %>% group_by(key) %>% 
    dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                     med.x  = quantile(value, probs = 0.5),
                     upper.x = quantile(value, probs = 0.75))
  tmpP$Category = uscene[s]
  dfT = rbind(dfT, tmpP) 
  rm(tmpD, tmpP)
}

ggplot(dfT, aes(x=as.numeric( as.character(key) ) , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  facet_wrap(~Category) +
  scale_x_log10() +  ylab("1-min PSD median") + xlab("HMD Frequency (Hz)")+
  ylim(c(50,95)) +   theme_bw()+ 
  ggtitle (paste("Soundscape Metrics by Scene")) +
  theme(text = element_text(size = 16) )

numeric_columns = grep("^\\d", names(HmdDets) )  
hix = names(HmdDets)[numeric_columns]
useason = unique(HmdDets$season)
uscene = unique(HmdDets$Category2)
dfT = NULL
for (s in 1:length(uscene)) {
  tmpD  = HmdDets %>% filter(Category2 == uscene[s])
  tmpP = tmpD %>% gather(key, value, numeric_columns) %>% group_by(key) %>% 
    dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                     med.x  = quantile(value, probs = 0.5),
                     upper.x = quantile(value, probs = 0.75))
  tmpP$Category2 = uscene[s]
  dfT = rbind(dfT, tmpP) 
  rm(tmpD, tmpP)
}

ggplot(dfT, aes(x=as.numeric( as.character(key) ) , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  facet_wrap(~Category2) +
  scale_x_log10() +  ylab("1-min PSD median") + xlab("HMD Frequency (Hz)")+
  ylim(c(50,95)) +   theme_bw()+ 
  ggtitle (paste("Soundscape Metrics by Scene")) +
  theme(text = element_text(size = 16) )

ggplot(dfT, aes(x=as.numeric( as.character(key) ) , y=med.x, color = Category2) )  +
  geom_line( linewidth = 2) +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1, linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, linetype="dotted") +
  #facet_wrap(~Category2) +
  scale_x_log10() +  ylab("1-min PSD median") + xlab("HMD Frequency (Hz)")+
  ylim(c(50,95)) +   theme_bw()+ 
  ggtitle (paste("Soundscape Metrics by Scene")) +
  theme(text = element_text(size = 16) )

talSeason = as.data.frame( HmdDets %>% group_by(Category2) %>% tally() )
category_labels <- paste(talSeason$Category2, "(n =", talSeason$n, ")")
names(category_labels) <- talSeason$Category2

ggplot(dfT, aes(x=as.numeric( as.character(key) ) , y=med.x, color = Category2) )  +
  geom_line( linewidth = 2) +
  geom_ribbon(aes(ymin = lower.x, ymax = upper.x, fill = Category2), alpha = 0.2, color = NA) +
  scale_x_log10() + xlab("HMD Frequency (Hz)")+
  ylab(expression("median 1-min PSD ("*mu*"P"^2*"/Hz)")) + 
   ylim(c(60,90)) +  xlim(c(100,1000)) + theme_bw()+ 
  #ggtitle (paste("Soundscape Metrics by Scene")) +
  theme(text = element_text(size = 16) )+
  labs(color = "SoundScape Scene", fill = "SoundScape Scene")  + # Changing the legend title
  # Update the legend labels with sample sizes
  scale_color_manual(labels = category_labels, values = scales::hue_pal()(length(category_labels))) +
  scale_fill_manual(labels = category_labels, values = scales::hue_pal()(length(category_labels)))


