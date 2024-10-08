# PLOT HMDdet

# by site results

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)
library(tidyverse)

inDir = (  "F:\\SanctSound\\analysis\\combineFiles_AcousticScene" )
inFiles = list.files( inDir, pattern = "HMDdetLF", full.names = T)
inFiles
pltf = 0
fqr = "LF"  #append this to output names

for (f in 1: length(inFiles)) { # f = 6 for testing
  
  load( inFiles[f])
  
  #FORMATTTING ####
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 2) #site name
  cat("Processing... ", st, "(", f, " of ", length(inFiles), ") \n")
  efq = ncol(HMDdet)-5
  fq = as.numeric(as.character( gsub("X","", colnames(HMDdet[2:efq] )) ) ) # Frequency range: truncate to 100-2000 Hz
  AStime = HMDdet[, efq:ncol(HMDdet) ]
  HMDdet$Day = as.Date( HMDdet$dateTime )
  
  # Acoustic Scenes over time ####
  # get start and end of different AS, so segment works for plotting
  HMDdet = HMDdet %>%  mutate(test = case_when(Category == lag(Category) ~ "No", TRUE ~ "Yes"))
  idx = which(HMDdet$test == "Yes") #start of new scene
  
  AS = NULL
  AS$Start    = HMDdet$dateTime[idx]
  AS$Category = HMDdet$Category[idx]
  AS$End      = c(  HMDdet$dateTime[ idx[2:length(idx) ] - 1 ] , HMDdet$dateTime[nrow(HMDdet)]  )
  AS$Hours    = as.numeric( as.character( difftime(AS$End, AS$Start, units = "hours") ))
  
  tal = as.data.frame( HMDdet %>% group_by(Category) %>% tally() )
  tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
  AS$mth = month(AS$Start)
  AS = as.data.frame (AS)
  
  ## PLOT 1: acoustic Scene over time ####
  p1 = ggplot(AS, aes(x=Start, xend=End, y=Category, yend=Category, color = Hours)) +
    geom_segment() +
    theme_bw()+ 
    scale_y_discrete(limits=rev)+ 
    geom_segment(size=10) +
    xlab("")+  ylab("")+ 
    ggtitle(paste("Acoustic Scenes at ", st, " (1-min)", sep = "") )  +
    labs(caption = (paste0("% time in each category: ", 
                           tal$Category[1], "=", tal$PerTime[1], " | ", 
                           tal$Category[2], "=", tal$PerTime[2], " | ",
                           tal$Category[3], "=", tal$PerTime[3], " | ",
                           tal$Category[4], "=", tal$PerTime[4] ))) +
    scale_color_gradientn(colours = viridis(10))+
    #facet_wrap(~mth) +
    theme(  axis.text.y = element_text(size = 10, colour="black"),
            axis.text.x = element_text(size = 10, colour="black"),
            plot.caption = element_text(size = 8) )
 
  p1
  ggsave(p1, file = paste0(inDir, "\\AcousticScene_",fqr, "_" , st,".png"), width = 1500, height = 700, units = "px")
  
  # why is there a long segment of ambient???-- it is real
  tmp = AS [AS$Hours >120, ]
  pt = ggplot(AS, aes(x=Start, y=Hours, color = Category)) +
    geom_point() 
  
  tmp2 = HMDdet [HMDdet$Day >= "2019-02-16" & HMDdet$Day < "2019-02-17", ]
  tmp2 = subset(tmp2, select= c('Day','dateTime','Category'))
  # just really long ambient sections... RRPCA breaks it up a bit
  
  # Median spectra for each day + Category ####
  udys = unique(HMDdet$Day)
  
  # single FQ
  tst = HMDdet %>% group_by(c(Category))  %>% summarise(median100 = median(X125, na.rm = TRUE))  
  HMDdet$Mth = month( HMDdet$dateTime )
  HMDdet %>% group_by(Category,Mth) %>%  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),  X125 = quantile(X125, c(0.25, 0.5, 0.75)))
  
  # all FQ
  endCol = ncol(HMDdet) - 7
  tst = HMDdet %>% gather(key, value, 2:endCol) %>% group_by(Category, key) %>% 
    dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                     mean.x = quantile(value, probs = 0.5),
                     upper.x = quantile(value, probs = 0.75))
  
  tst$Fq = as.numeric(as.character( gsub("X","", tst$key ) ) )
  tst    = as.data.frame(tst)
  
  ## PLOT 2: median spectra ####
  p2 = ggplot(tst, aes(x=Fq , y=mean.x, color = Category) )  +
    geom_line( size = 1) +
    scale_x_log10() +
    ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
    ylim(c(50,100)) +
    theme_bw()+ 
    ggtitle(paste0(st, " (only low-frequency sources)")) +
    theme(legend.position="top")+
    theme(text = element_text(size =10) )
  p2
  ggsave(p2, file = paste0(inDir, "\\ASspectra_",fqr, "_", st,".png"), width = 1500, height = 1000, units = "px")
  
  ## PLOT 3: median + percentile spectra ####
  p3 =  ggplot(tst, aes(x=Fq, color=Category ) ) +
    geom_line(aes (y=mean.x), size = 1   ) + 
    geom_line(aes (y=lower.x),  alpha = .5,  size = 1 ) + #linetype="dotted",
    geom_line(aes (y=upper.x),  alpha = .5,  size = 1 ) +
    scale_x_log10() +
    ylab("1-min HMD Percentiles (25,50,95)")+ xlab("Frequency (Hz)")+
    theme_bw()+ 
    ylim(c(50,100)) +
    ggtitle( paste0( st, ": ", as.character( min(HMDdet$Day) ), " to ", as.character( max(HMDdet$Day) ), "\n (Total min=", nrow(HMDdet),")" )) +
    theme(legend.position="top")+ 
    theme(text = element_text(size =10) )
  p3
  ggsave(p3, file = paste0(inDir, "\\ASspectraPer_",fqr, "_", st,".png"), width = 1500, height = 1000, units = "px")
  
  DC = Sys.Date()
  save(tst, file = paste0(inDir, "\\HMDdetLF_Spectra_", st, "_", DC, ".Rda") )
  
  # Labeled spectra- one day ####
  
  dy = "2019-04-11"
  tmpD = HMDdet[ HMDdet$Day == dy, ] 
  head(tmpD)
  ## PLOT extra: spectra for a given day ####
  if (pltf == 1) {
    
    medSPLm = reshape::melt (tmpD, id.vars = c("dateTime", "Category"),  measure.vars = colnames(tmpD)[ 2 : endCol ] )
    colnames( medSPLm)  = c("date", "AS", "Fq", "SPL")
    medSPLm$Fq = as.numeric(as.character( gsub("X","", medSPLm$Fq ) ) ) #head(medSPLm)
    
    ggplot(medSPLm, aes(x=Fq, y=SPL, group = date ) ) +
      geom_line(alpha = .2 ) + 
      scale_x_log10() +
      ylab("1-min HMD")+ xlab("Frequency (Hz)")+
      facet_wrap(~AS)+
      theme_minimal() +
      ggtitle(paste0( st, " on ", dy) ) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(text = element_text(size =20) )
  }
  
}


