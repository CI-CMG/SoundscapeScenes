# PLOT HMDdet

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

inDir = "G:\\My Drive\\ActiveProjects\\SANCTSOUND\\combineFiles_AcousticScene\\"
inFile = choose.files()
pltf = 0

load( inFile)
as.data.frame( HMDdet %>% group_by(Category) %>% tally() )


# START HERE: 
# get start and end of different , so segment works for plotting ####
ggplot(outputVD, aes(x=Start, xend=End, y=Category, yend=Category, color=`TOL_125 max`)) +
  geom_segment()+
  theme_bw()+ 
  scale_y_discrete(limits=rev)+ 
  geom_segment(size=12) +
  xlab("")+  ylab("")+ ggtitle("Summary of Vessel Detection Periods") +
  labs(caption = (paste0("samples in each category: A=", tal$n[1]," | B=", tal$n[2]," | C=", tal$n[3]," | D=", tal$n[4] )))+
  scale_color_gradientn(colours = viridis(10))+
  theme(  axis.text.y = element_text(size = 14, colour="black"),
          axis.text.x=element_text(size = 14, colour="black"),
          plot.caption = element_text(size = 14) )

# plot labeled spectra-- select specific data
HMDdet$Day = as.Date( HMDdet$dateTime )
tmpD = HMDdet[ HMDdet$Day == "2019-04-28", ]
ed = ncol(tmpD) -4

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