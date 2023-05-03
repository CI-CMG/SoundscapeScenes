# PLOT HMDdet

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)

inDir = "G:\\My Drive\\ActiveProjects\\SANCTSOUND\\combineFiles_AcousticScene\\"
inFile = choose.files()
pltf = 0

load( inFile)

#FORMATTTING ####
st =  sapply(strsplit(basename( inFile), "_"), "[[", 2) #site name
dpl = gsub(".Rda", "", sapply(strsplit(basename( inFile ), "_"), "[[", 3) ) # deployment name
efq = ncol(HMDdet)-5
fq = as.numeric(as.character( gsub("X","", colnames(HMDdet[2:efq] )) ) ) # Frequency range: truncate to 100-2000 Hz

# Acoustic Scenes over time ####
# get start and end of different AS, so segment works for plotting
HMDdet = HMDdet %>%  mutate(test = case_when(Category == lag(Category) ~ "No", TRUE ~ "Yes"))
HMDdet$test
idx = which(HMDdet$test == "Yes") #start of new scene
rm(AS)
AS = as.data.frame(matrix(nrow = length(idx), ncol=3))
AS$Start = HMDdet$dateTime[idx]
AS$Category= HMDdet$Category[idx]
AS$End = c( HMDdet$dateTime[ idx[2:length(idx)] +1 ] , HMDdet$dateTime[nrow(HMDdet)] )
AS$Hours = as.numeric( as.character( difftime(AS$End, AS$Start, units = "hours") ))
tal = as.data.frame( HMDdet %>% group_by(Category) %>% tally() )
tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
AS$mth = month(AS$Start)

ggplot(AS, aes(x=Start, xend=End, y=Category, yend=Category, color = Hours)) +
  geom_segment() +
  theme_bw()+ 
  scale_y_discrete(limits=rev)+ 
  geom_segment(size=20) +
  xlab("")+  ylab("")+ ggtitle(paste("Acoustic Scenes at ", st, " (1-min)", sep = "") )  +
  labs(caption = (paste0("Percent time in each category: ", 
                         tal$Category[1], "=", tal$PerTime[1], " | ", 
                         tal$Category[2], "=", tal$PerTime[2], " | ",
                         tal$Category[3], "=", tal$PerTime[3], " | ",
                         tal$Category[4], "=", tal$PerTime[4] ))) +
  scale_color_gradientn(colours = viridis(10))+
  #facet_wrap(~mth) +
  theme(  axis.text.y = element_text(size = 14, colour="black"),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.caption = element_text(size = 14) )

# Labeled spectra ####
HMDdet$Day = as.Date( HMDdet$dateTime )
tmpD = HMDdet[ HMDdet$Day == "2019-05-11", ]
ed = ncol(tmpD) - 7

if (pltf == 1) {
  medSPLm = reshape::melt (tmpD, id.vars = c("dateTime", "Category"),  measure.vars = colnames(tmpD)[ 2 : ed ] )
  colnames( medSPLm)  = c("date", "AS", "Fq", "SPL")
  medSPLm$Fq = as.numeric(as.character( gsub("X","", medSPLm$Fq ) ) ) #head(medSPLm)
  
  ggplot(medSPLm, aes(x=Fq, y=SPL, group = date ) ) +
    geom_line(alpha = .2 ) + 
    scale_x_log10() +
    ylab("1-min HMD")+ xlab("Frequency (Hz)")+
    facet_wrap(~AS)+
    theme_minimal() +
    #ggtitle(paste0( st, " on ", dy) ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}